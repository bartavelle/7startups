module Main where

import Startups.Base

import Criterion
import Criterion.Main

import qualified Data.MultiSet as MS
import qualified RMultiSet as RS

smallList :: [Resource]
smallList = [Hype, Operations]

largeList :: [Resource]
largeList = concat (replicate 100 [minBound .. maxBound])

main :: IO ()
main = defaultMain
  [ bgroup "fromList"
    [ bench "multiset - small" $ whnf MS.fromList smallList
    , bench "resource - small" $ whnf RS.fromList smallList
    , bench "multiset - large" $ whnf MS.fromList largeList
    , bench "resource - large" $ whnf RS.fromList largeList
    ]
  ]
