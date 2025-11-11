module Backends.Pure where

import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Startups.Base
import Startups.Game
import Startups.GameTypes
import Startups.Interpreter
import Startups.Utils
import System.Random

pureDict :: Strategy Identity (State StdGen) -> OperationDict Identity (State StdGen)
pureDict st = OperationDict st (return . Right . runIdentity) msg
  where
    msg _ _ = return ()

runPure :: Strategy Identity (State StdGen) -> StdGen -> GameState -> GameMonad Identity a -> (GameState, Either Message a)
runPure st g gs o = evalState (runInterpreter (pureDict st) gs o) g

pureGame :: Strategy Identity (State StdGen) -> StdGen -> [PlayerId] -> (GameState, Either Message (M.Map PlayerId (M.Map VictoryType VictoryPoint)))
pureGame st g playerlist =
  let gs = initialGameState g1 playerlist
      (g1, g2) = split g
   in runPure st g2 gs playGame
