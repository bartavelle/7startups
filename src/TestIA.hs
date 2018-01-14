{-# LANGUAGE OverloadedStrings #-}
module Main where

import Backends.Pure
import Strategies.Random
import Strategies.Bot1
import Strategies.Compose

import Control.Lens
import System.Random
import Control.Monad
import Data.Monoid
import Data.Tuple (swap)
import Data.List (sort, foldl')
import qualified Data.Map.Strict as M
import Control.Parallel.Strategies

import Prelude

main :: IO ()
main = do
    let strat = composeStrat stdGenStateStrat [("bot1", bot1 pure)]
    stds <- replicateM 1000 newStdGen
    let roundR rnd = case pureGame strat rnd ["bot1", "rnd1", "rnd2", "rnd4"] of
                          (_, Left rr) -> error (show rr)
                          (_, Right m) -> let scores = m & traverse %~ view folded & itoList & map swap & sort & reverse
                                          in  case scores of
                                                  ((_,pid):_) -> M.singleton pid (1 :: Int)
                                                  _ -> mempty
        res = foldl' (M.unionWith (+)) mempty $ withStrategy (parListChunk 50 rpar) $ map roundR stds
    print res

