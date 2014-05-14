{-# LANGUAGE GADTs #-}
module Startups.Interpreter where

import Startups.Base
import Startups.Cards
import Startups.GameTypes

import qualified Data.Map.Strict as M
import Control.Monad.Operational
import Control.Monad.State.Strict

data OperationDict m = OperationDict { _doPlayerDecision :: Age -> Turn -> PlayerId -> NonEmpty Card -> GameState -> m (PlayerAction, Exchange)
                                     , _doAskCard        :: Age -> PlayerId -> NonEmpty Card -> GameState -> Message -> m Card
                                     , _doActionsRecap   :: GameState -> M.Map PlayerId (PlayerAction, Exchange) -> m ()
                                     , _doTellPlayer     :: PlayerId -> Message -> m ()
                                     , _doGeneralMessage :: Message -> m ()
                                     }

runInterpreter :: Monad m
               => OperationDict m
               -> GameState
               -> GameMonad a
               -> m (GameState, Either Message a)
runInterpreter dico gamestate m =
    case runState (viewT m) gamestate of
        (a, nextstate) -> evalInstrGen dico nextstate a

evalInstrGen :: Monad m
             => OperationDict m
             -> GameState
             -> ProgramViewT GameInstr (State GameState) a
             -> m (GameState, Either Message a)
evalInstrGen _ gamestate (Return x) = return (gamestate, Right x)
evalInstrGen dico gamestate (a :>>= f) =
    let runC a' = runInterpreter dico gamestate (f a')
    in  case a of
            PlayerDecision age turn pid clist -> _doPlayerDecision dico age turn pid clist gamestate >>= runC
            AskCard age pid cards msg -> _doAskCard dico age pid cards gamestate msg >>= runC
            ActionsRecap actions -> _doActionsRecap dico gamestate actions >>= runC
            TellPlayer pid msg -> _doTellPlayer dico pid msg >>= runC
            GeneralMessage msg -> _doGeneralMessage dico msg >>= runC
            ThrowError err -> return (gamestate, Left err)
            CatchError n handler -> do
                n' <- runInterpreter dico gamestate n
                case n' of
                    (gs', Left rr) -> runInterpreter dico gs' (handler rr >>= f)
                    (gs', Right x) -> runInterpreter dico gs' (f x)
