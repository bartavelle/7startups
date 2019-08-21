{-| Use the `Model` - `View` - `Controller` pattern to separate impure inputs
    and outputs from pure application logic so that you can:

    * Equationally reason about your model

    * Exercise your model with property-based testing (like @QuickCheck@)

    * Reproducibly replay your model

    The @mvc@ library uses the type system to statically enforce the separation
    of impure `View`s and `Controller`s from the pure `Model`.

    Here's a small example program written using the @mvc@ library to illustrate
    the core types and concepts:

> import MVC
> import qualified MVC.Prelude as MVC
> import qualified Pipes.Prelude as Pipes
>
> external :: Managed (View String, Controller String)
> external = do
>     c1 <- MVC.stdinLines
>     c2 <- MVC.tick 1
>     return (MVC.stdoutLines, c1 <> fmap show c2)
>
> model :: Model () String String
> model = asPipe (Pipes.takeWhile (/= "quit"))
>     
> main :: IO ()
> main = runMVC () model external

    This program has three components:

    * A `Controller` that interleaves lines from standard input with periodic
      ticks

    * A `View` that writes lines to standard output

    * A pure `Model`, which forwards lines until the user inputs \"quit\"

    'runMVC' connects them into a complete program, which outputs a @()@ every
    second and also echoes standard input to standard output until the user
    enters \"quit\":

>>> main
()
Test<Enter>
Test
()
()
42<Enter>
42
()
quit<enter>
>>>

    The following sections give extended guidance for how to structure @mvc@
    programs.  Additionally, there is an "MVC.Prelude" module, which provides
    several utilities and provides a more elaborate code example using the
    @sdl@ library.
-}

{-# LANGUAGE RankNTypes #-}

module MVC (
    -- * Controllers
    -- $controller
      Controller
    , asInput
    , keeps

    -- * Views
    -- $view
    , View
    , asSink
    , asFold
    , handles

    -- * Models
    -- $model
    , Model
    , ModelM
    , asPipe

    -- * MVC
    -- $mvc
    , runMVC
    , generalizeMVC

    -- * Managed resources
    -- $managed
    , Managed
    , managed

    -- *ListT
    , loop
    -- $listT

    -- * Re-exports
    -- $reexports
    , module Data.Functor.Constant
    , module Data.Functor.Contravariant
    , module Data.Monoid
    , module Pipes
    , module Pipes.Concurrent
    ) where

import Control.Category (Category(..))
import Control.Foldl (FoldM(..), HandlerM, impurely, premapM)
import qualified Control.Foldl as Fold
import Control.Monad.Managed (Managed, managed, with)
import Control.Monad.Morph (generalize)
import Control.Monad.Trans.State.Strict (execStateT, StateT)
import Data.Functor.Constant (Constant(Constant, getConstant))
import Data.Functor.Contravariant (Contravariant(contramap))
import Data.Monoid (Monoid(mempty, mappend, mconcat), (<>), First)
import qualified Data.Monoid as M
import qualified Data.Semigroup as S
import Pipes
import Pipes.Concurrent
import Pipes.Prelude (foldM, loop)
import Data.Functor.Identity (Identity)

import Prelude hiding ((.), id)

{- $controller
    `Controller`s represent concurrent inputs to your system.  Use the `Functor`
    and `Monoid` instances for `Controller` and `Managed` to unify multiple
    `Managed` `Controller`s together into a single `Managed` `Controller`:

> controllerA :: Managed (Controller A)
> controllerB :: Managed (Controller B)
> controllerC :: Managed (Controller C)
>
> data TotalInput = InA A | InB B | InC C
>
> controllerTotal :: Managed (Controller TotalInput)
> controllerTotal =
>         fmap (fmap InA) controllerA
>     <>  fmap (fmap InB) controllerB
>     <>  fmap (fmap InC) controllerC

    Combining `Controller`s interleaves their values.
-}

{-| A concurrent source

> fmap f (c1 <> c2) = fmap f c1 <> fmap f c2
>
> fmap f mempty = mempty
-}
newtype Controller a = AsInput (Input a)
-- This is just a newtype wrapper around `Input` because:
--
-- * I want the `Controller` name to "stick" in inferred types
--
-- * I want to restrict the API to ensure that `runMVC` is the only way to
--   consume `Controller`s.  This enforces strict separation of `Controller`
--   logic from `Model` or `View` logic

-- Deriving `Functor`
instance Functor Controller where
    fmap f (AsInput i) = AsInput (fmap f i)

-- Deriving `Semigroup`
instance S.Semigroup (Controller a) where
    (AsInput i1) <> (AsInput i2) = AsInput (i1 S.<> i2)

-- Deriving `Monoid`
instance Monoid (Controller a) where
    mappend = (<>)

    mempty = AsInput mempty

-- | Create a `Controller` from an `Input`
asInput :: Input a -> Controller a
asInput = AsInput
{-# INLINABLE asInput #-}

{-| Think of the type as one of the following types:

> keeps :: Prism'     a b -> Controller a -> Controller b
> keeps :: Traversal' a b -> Controller a -> Controller b

    @(keeps prism controller)@ only emits values if the @prism@ matches the
    @controller@'s output.

> keeps (p1 . p2) = keeps p2 . keeps p1
>
> keeps id = id

> keeps p (c1 <> c2) = keeps p c1 <> keeps p c2
>
> keeps p mempty = mempty
-}
keeps
    :: ((b -> Constant (First b) b) -> (a -> Constant (First b) a))
    -- ^
    -> Controller a
    -- ^
    -> Controller b
keeps k (AsInput (Input recv_)) = AsInput (Input recv_')
  where
    recv_' = do
        ma <- recv_
        case ma of
            Nothing -> return Nothing
            Just a  -> case match a of
                Nothing -> recv_'
                Just b  -> return (Just b)
    match = M.getFirst . getConstant . k (Constant . M.First . Just)
{-# INLINABLE keeps #-}

{- $view
    `View`s represent outputs of your system.  Use `handles` and the `Monoid`
    instance of `View` to unify multiple `View`s together into a single `View`:

> viewD :: Managed (View D)
> viewE :: Managed (View E)
> viewF :: Managed (View F)
>
> data TotalOutput = OutD D | OutE E | OutF F
>
> makePrisms ''TotalOutput  -- Generates _OutD, _OutE, and _OutF prisms
>
> viewTotal :: Managed (View TotalOutput)
> viewTotal =
>         fmap (handles _OutD) viewD
>     <>  fmap (handles _OutE) viewE
>     <>  fmap (handles _OutF) viewF

    Combining `View`s sequences their outputs.

    If a @lens@ dependency is too heavy-weight, then you can manually generate
    `Traversal`s, which `handles` will also accept.  Here is an example of how
    you can generate `Traversal`s by hand with no dependencies:

> -- _OutD :: Traversal' TotalOutput D
> _OutD :: Applicative f => (D -> f D) -> (TotalOutput -> f TotalOutput)
> _OutD k (OutD d) = fmap OutD (k d)
> _OutD k  t       = pure t
>
> -- _OutE :: Traversal' TotalOutput E
> _OutE :: Applicative f => (E -> f E) -> (TotalOutput -> f TotalOutput)
> _OutE k (OutE d) = fmap OutE (k d)
> _OutE k  t       = pure t
>
> -- _OutF :: Traversal' TotalOutput F
> _OutF :: Applicative f => (F -> f F) -> (TotalOutput -> f TotalOutput)
> _OutF k (OutF d) = fmap OutF (k d)
> _OutF k  t       = pure t
-}

{-| An effectful sink

> contramap f (v1 <> v2) = contramap f v1 <> contramap f v2
>
> contramap f mempty = mempty
-}
newtype View a = AsFold (FoldM IO a ())

instance S.Semigroup (View a) where
    (AsFold fold1) <> (AsFold fold2) = AsFold (fold1 S.<> fold2)

instance Monoid (View a) where
    mempty = AsFold mempty
    mappend = (<>)

instance Contravariant View where
    contramap f (AsFold fold) = AsFold (premapM (return . f) fold)

-- | Create a `View` from a sink
asSink :: (a -> IO ()) -> View a
asSink sink = AsFold (FoldM step begin done)
  where
    step x a = do
        sink a
        return x
    begin = return ()
    done = return
{-# INLINABLE asSink #-}

-- | Create a `View` from a `FoldM`
asFold :: FoldM IO a () -> View a
asFold = AsFold
{-# INLINABLE asFold #-}

{-| Think of the type as one of the following types:

> handles :: Prism'     a b -> View b -> View a
> handles :: Traversal' a b -> View b -> View a

    @(handles prism view)@ only runs the @view@ if the @prism@ matches the
    input.

> handles (p1 . p2) = handles p1 . handles p2
>
> handles id = id

> handles p (v1 <> v2) = handles p v1 <> handles p v2
>
> handles p mempty = mempty
-}
handles
    :: HandlerM IO a b
    -- ^
    -> View b
    -- ^
    -> View a
handles k (AsFold fold) = AsFold (Fold.handlesM k fold)
{-# INLINABLE handles #-}

{- $model
    `Model`s are stateful streams and they sit in between `Controller`s and
    `View`s.

    Use `State` to internally communicate within the `Model`.

    Read the \"ListT\" section which describes why you should prefer `ListT`
    over `Pipe` when possible.

    Also, try to defer converting your `Pipe` to a `Model` until you call
    `runMVC`, because the conversion is not reversible and `Pipe` is strictly
    more featureful than `Model`.
-}

{-| A @(Model s a b)@ converts a stream of @(a)@s into a stream of @(b)@s while
    interacting with a state @(s)@
-}
newtype ModelM m s a b = AsPipe (Pipe a b (StateT s m) ())
type Model = ModelM Identity

instance Monad m => Category (ModelM m s) where
    (AsPipe m1) . (AsPipe m2) = AsPipe (m1 <-< m2)

    id = AsPipe cat

{-| Create a `Model` from a `Pipe`

> asPipe (p1 <-< p2) = asPipe p1 . asPipe p2
>
> asPipe cat = id
-}
asPipe :: Pipe a b (StateT s m) () -> ModelM m s a b
asPipe = AsPipe
{-# INLINABLE asPipe #-}

{- $mvc
    Connect a `Model`, `View`, and `Controller` and an initial state
    together using `runMVC` to complete your application.

    `runMVC` is the only way to consume `View`s and `Controller`s.  The types
    forbid you from mixing `View` and `Controller` logic with your `Model`
    logic.

    Note that `runMVC` only accepts one `View` and one `Controller`.  This
    enforces a single entry point and exit point for your `Model` so that you
    can cleanly separate your `Model` logic from your `View` logic and
    `Controller` logic.  The way you add more `View`s and `Controller`s to your
    program is by unifying them into a single `View` or `Controller` by using
    their `Monoid` instances.  See the \"Controllers\" and \"Views\" sections
    for more details on how to do this.
-}

{-| Connect a `Model`, `View`, and `Controller` and initial state into a
    complete application.
-}

runMVC :: s
    -- ^ Initial state
    -> Model s a b
    -- ^ Program logic
    -> Managed (View b, Controller a)
    -- ^ Effectful output and input
    -> IO s
    -- ^ Returns final state
runMVC = generalizeMVC generalize

{-| Connect a `Model`, `View`, and `Controller` and initial state into a
    complete application over arbitrary monad given a morphism to IO.
-}
generalizeMVC
    :: Monad m => (forall x . m x -> IO x)
    -- ^ Monad morphism
    -> s
    -- ^ Initial state
    -> ModelM m s a b
    -- ^ Program logic
    -> Managed (View b, Controller a)
    -- ^ Effectful output and input
    -> IO s
    -- ^ Returns final state
generalizeMVC cb initialState (AsPipe pipe) viewController =
    with viewController $ \(AsFold (FoldM step begin done), AsInput input) -> do
    let step' x a = lift (step x a)
    let begin'    = lift begin
    let done'  x  = lift (done x)
    let fold' = FoldM step' begin' done'
    flip execStateT initialState $
        impurely foldM fold' (fromInput input >-> hoist (hoist cb) pipe)
{-# INLINABLE runMVC #-}

{- $managed
    Use `managed` to create primitive `Managed` resources and use the `Functor`,
    `Applicative`, `Monad`, and `Monoid` instances for `Managed` to bundle
    multiple `Managed` resources into a single `Managed` resource.

    See the source code for the \"Utilities\" section below for several examples
    of how to create `Managed` resources.
-}

{- $listT
    `ListT` computations can be combined in more ways than `Pipe`s, so try to
    program in `ListT` as much as possible and defer converting it to a `Pipe`
    as late as possible using `loop`.

    You can combine `ListT` computations even if their inputs and outputs are
    completely different:

> -- Independent computations
>
> modelAToD :: A -> ListT (State S) D
> modelBToE :: B -> ListT (State S) E
> modelCToF :: C -> ListT (State s) F
>
> modelInToOut :: TotalInput -> ListT (State S) TotalOutput
> modelInToOut totalInput = case totalInput of
>     InA a -> fmap OutD (modelAToD a)
>     InB b -> fmap OutE (modelBToE b)
>     InC c -> fmap OutF (modelCToF c)

    Sometimes you have multiple computations that handle different inputs but
    the same output, in which case you don't need to unify their outputs:

> -- Overlapping outputs
>
> modelAToOut :: A -> ListT (State S) Out
> modelBToOut :: B -> ListT (State S) Out
> modelCToOut :: C -> ListT (State S) Out
>
> modelInToOut :: TotalInput -> ListT (State S) TotalOutput
> modelInToOut totalInput = case totalInput of
>     InA a -> modelAToOut a
>     InB b -> modelBToOut b
>     InC c -> modelCToOut c

    Other times you have multiple computations that handle the same input but
    produce different outputs.  You can unify their outputs using the `Monoid`
    and `Functor` instances for `ListT`:

> -- Overlapping inputs
>
> modelInToA :: TotalInput -> ListT (State S) A
> modelInToB :: TotalInput -> ListT (State S) B
> modelInToC :: TotalInput -> ListT (State S) C
>
> modelInToOut :: TotalInput -> ListT (State S) TotalOutput
> modelInToOut totalInput =
>        fmap OutA (modelInToA totalInput)
>     <> fmap OutB (modelInToB totalInput)
>     <> fmap OutC (modelInToC totalInput)

    You can also chain `ListT` computations, feeding the output of the first
    computation as the input to the next computation:

> -- End-to-end
>
> modelInToMiddle  :: TotalInput -> ListT (State S) MiddleStep
> modelMiddleToOut :: MiddleStep -> ListT (State S) TotalOutput
>
> modelInToOut :: TotalInput -> ListT (State S) TotalOutput
> modelInToOut = modelInToMiddle >=> modelMiddleToOut

    ... or you can just use @do@ notation if you prefer.

    However, the `Pipe` type is more general than `ListT` and can represent
    things like termination.  Therefore you should consider mixing `Pipe`s with
    `ListT` when you need to take advantage of these extra features:

> -- Mix ListT with Pipes
>
> pipe :: Pipe TotalInput TotalOutput (State S) ()
> pipe = Pipes.takeWhile (not . isC)) >-> loop modelInToOut
>   where
>     isC (InC _) = True
>     isC  _      = False

    So promote your `ListT` logic to a `Pipe` when you need to take advantage of
    these `Pipe`-specific features.
-}

{- $reexports
    "Data.Functor.Constant" re-exports `Constant`

    "Data.Functor.Contravariant" re-exports `Contravariant`

    "Data.Monoid" re-exports `Monoid`, (`<>`), `mconcat`, and `First` (the type
    only)

    "Pipes" re-exports everything

    "Pipes.Concurrent" re-exports everything
-}
