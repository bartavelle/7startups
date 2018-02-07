{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Strategies.Random where

import Startups.Interpreter
import Startups.GameTypes
import Startups.Utils
import qualified Data.List.NonEmpty as NE

import Control.Monad.State.Strict
import System.Random (randomR, StdGen)
import Control.Lens
import Control.Applicative
import Prelude

randStrategy :: forall p m. (Monad m, Functor m) => (forall a. a -> p a) -> (Int -> Int -> m Int) -> Strategy p m
randStrategy pureP roll = Strategy pd ac
    where
        pd age _ pid necards stt = do
            let pm = stt ^. playermap
                allactions = NE.toList $ allowableActions age pid necards pm
                nodrops = filter (\(PlayerAction actiontype _,_,_) -> actiontype /= Drop) allactions
                actions = if null nodrops then allactions else nodrops
            pureP . (actions !!) <$> roll 0 (length actions - 1)
        ac _ _ necards _ _ = do
            let cards = _NonEmpty # necards
            (pureP . (cards !!)) <$> roll 0 (length cards - 1)

stdGenStateStrat :: forall p m. (Monad p, MonadState StdGen m) => (forall a. a -> p a) -> Strategy p m
stdGenStateStrat pureP = randStrategy pureP $ \mi mx -> do
    g <- get
    let (o,g') = randomR (mi, mx) g
    put g'
    return o
