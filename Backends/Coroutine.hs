{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Backends.Coroutine where

import Control.Monad
import Data.Bifunctor
import Data.List.NonEmpty
import Control.Monad.State.Strict

import Startups.Interpreter
import Startups.Base
import Startups.Cards
import Startups.GameTypes

type Y = (,)

newtype Coroutine s m r = Coroutine { resume :: m (Either ((Y s) (Coroutine s m r)) r) }

nfmap :: (Functor m) => (a -> b) -> Either ((Y s) (Coroutine s m a)) a -> Either ((Y s) (Coroutine s m b)) b
nfmap f = bimap (fmap (fmap f)) f

instance (Functor m) => Functor (Coroutine s m) where
    fmap f = Coroutine . fmap (nfmap f) . resume

instance Monad m => Applicative (Coroutine s m) where
    pure = Coroutine . pure . Right
    (<*>) = ap

instance Monad m => Monad (Coroutine s m) where
    t >>= f = Coroutine (resume t >>= apply f)
        where
            apply a (Right x) = resume (a x)
            apply a (Left y) = return (Left (fmap (>>= a) y))

instance MonadTrans (Coroutine s) where
    lift = Coroutine . fmap Right

instance MonadIO m => MonadIO (Coroutine s m) where
    liftIO = lift . liftIO

suspend :: Monad m => (s, Coroutine s m r) -> Coroutine s m r
suspend s = Coroutine (return (Left s))

yield :: Monad m => x -> Coroutine x m ()
yield x = suspend (x, return ())

data AP = AP Age Turn PlayerId (NonEmpty Card) GameState
data AC = AC Age PlayerId (NonEmpty Card) GameState Message

data Com p = CAP AP (p (PlayerAction, Exchange))
           | CAC AC (p Card)
           | MSG GameState CommunicationType

runCR :: Monad m => (forall x. p x -> m x)
                 -> (forall x. m (p x))
                 -> GameState
                 -> GameMonad p a
                 -> Coroutine (Com p) m (GameState, Either Message a)
runCR getp mkp = runInterpreter dict
    where
        dict = OperationDict (Strategy pa ac) (fmap Right . lift . getp) (\gs -> yield . MSG gs)
        pa age turn pid clist gs = do
            p <- lift mkp
            yield (CAP (AP age turn pid clist gs) p)
            return p
        ac age pid cards gs msg = do
            p <- lift mkp
            yield (CAC (AC age pid cards gs msg) p)
            return p
