module Strategies.Random where

import Startups.Interpreter
import Startups.GameTypes
import Startups.Utils
import qualified Data.List.NonEmpty as NE

import Control.Lens
import Control.Applicative

randStrategy :: (Monad m, Monad p, Functor m) => (Int -> Int -> m Int) -> Strategy p m
randStrategy roll = Strategy pd ac
    where
        pd age _ pid necards stt = do
            let pm = stt ^. playermap
                x = allowableActions age pid necards pm
            return . (\(pa,e,_) -> (pa,e)) . (x NE.!!) <$> roll 0 (NE.length x - 1)
        ac _ _ necards _ _ = do
            let cards = _NonEmpty # necards
            (return . (cards !!)) <$> roll 0 (length cards - 1)

