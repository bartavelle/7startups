{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Startups.Interpreter where

import Startups.Base
import Startups.Cards
import Startups.GameTypes

import Control.Monad.Operational
import Control.Monad.State.Strict
import Data.List.NonEmpty

data Strategy p m = Strategy { _doPlayerDecision :: Age -> Turn -> PlayerId -> NonEmpty Card -> GameState -> m (p (PlayerAction, Exchange, Maybe SpecialInformation))
                             , _doAskCard        :: Age -> PlayerId -> NonEmpty Card -> GameState -> Message -> m (p Card)
                             }

data OperationDict p m = OperationDict { _strat        :: Strategy p m
                                       , _doGetPromise :: forall a. p a -> m (Either Message a)
                                       , _doMessage    :: GameState -> CommunicationType -> m ()
                                       }

data OperationDict' p m = OperationDict' { _strat'           :: Strategy p m
                                         , _doGetPromiseAct  :: p (PlayerAction, Exchange, Maybe SpecialInformation) -> m (Either Message (PlayerAction, Exchange, Maybe SpecialInformation))
                                         , _doGetPromiseCard :: p Card -> m (Either Message Card)
                                         , _doMessage'       :: GameState -> CommunicationType -> m ()
                                         }
runInterpreter :: Monad m
               => OperationDict p m
               -> GameState
               -> GameMonad p a
               -> m (GameState, Either Message a)
runInterpreter dico = runInterpreter' dico'
    where
        dico' = OperationDict' (_strat dico) (_doGetPromise dico) (_doGetPromise dico) (_doMessage dico)

runInterpreter' :: Monad m
                => OperationDict' p m
                -> GameState
                -> GameMonad p a
                -> m (GameState, Either Message a)
runInterpreter' dico gamestate m =
    case runState (viewT m) gamestate of
        (a, nextstate) -> evalInstrGen' dico nextstate a

evalInstrGen' :: forall p m a. Monad m
              => OperationDict' p m
              -> GameState
              -> ProgramViewT (GameInstr p) (State GameState) a
              -> m (GameState, Either Message a)
evalInstrGen' _ gamestate (Return x) = return (gamestate, Right x)
evalInstrGen' dico gamestate (a :>>= f) =
    let runC a' = runInterpreter' dico gamestate (f a')
    in  case a of
            PlayerDecision age turn pid clist -> _doPlayerDecision (_strat' dico) age turn pid clist gamestate >>= runC
            GetPromiseCard x -> do
                o <- _doGetPromiseCard dico x
                case o of
                    Left rr -> return (gamestate, Left rr)
                    Right v -> runC v
            GetPromiseAct x -> do
                o <- _doGetPromiseAct dico x
                case o of
                    Left rr -> return (gamestate, Left rr)
                    Right v -> runC v
            AskCard age pid cards msg -> _doAskCard (_strat' dico) age pid cards gamestate msg >>= runC
            Message com -> _doMessage' dico gamestate com >>= runC
            ThrowError err -> return (gamestate, Left err)
            CatchError n handler -> do
                n' <- runInterpreter' dico gamestate n
                case n' of
                    (gs', Left rr) -> runInterpreter' dico gs' (handler rr >>= f)
                    (gs', Right x) -> runInterpreter' dico gs' (f x)
