module Backends.Pure where

import Startups.GameTypes
import Startups.Base
import Startups.Game
import Startups.Interpreter
import Startups.Utils
import Control.Monad.State.Strict
import System.Random
import Control.Lens
import qualified Data.Map.Strict as M

pureDict :: OperationDict Identity (State StdGen)
pureDict = OperationDict (Strategy pd ac) (return . Right . runIdentity) msg
    where
        roll :: Int -> State StdGen Int
        roll x = do
            g <- get
            let (o,g') = randomR (0, x - 1) g
            put g'
            return o
        pd age _ pid necards stt = do
            let cards = _NonEmpty # necards
                x = allowableActions age pid cards (stt ^. playermap)
            n <- roll (length x)
            let (pe,e,_) = x !! n
            return (return (pe, e))
        ac _ _ necards _ _ =
            let lcards = _NonEmpty # necards
            in  fmap (lcards !!) (roll (length lcards))
        msg _ _ = return ()

runPure :: StdGen -> GameState -> GameMonad Identity a -> (GameState, Either Message a)
runPure g gs o = evalState (runInterpreter pureDict gs o) g

pureGame :: StdGen -> [PlayerId] -> (GameState, Either Message (M.Map PlayerId (M.Map VictoryType VictoryPoint)))
pureGame g playerlist =
    let gs = initialGameState g1 playerlist
        (g1, g2) = split g
    in  runPure g2 gs playGame
