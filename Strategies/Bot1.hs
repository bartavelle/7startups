{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module Strategies.Bot1 where

import Startups.Interpreter
import Startups.GameTypes
import Startups.Cards
import Startups.Base
import Startups.Utils

import Control.Monad.State.Strict
import Control.Lens
import qualified Data.IntMap as IM
import qualified Data.List.NonEmpty as NE
import qualified Data.MultiSet as MS
import qualified Data.Map.Strict as M
import Data.Set.Lens
import qualified Data.Set as S
import Data.List
import Prelude

data StratState = StratState { _knownHands :: IM.IntMap [[Card]]
                             , _prevState :: GameState
                             }

makeLenses ''StratState

priority :: Age -> [ PlayerAction -> Exchange -> Maybe SpecialInformation -> PlayerId -> GameState -> Maybe Int ]
priority a = case a of
                 Age3 -> nodrop : buildcomp : map withPlay [military, points]
                 _ -> nodrop : buildcomp : map withPlay [resources, military, points]
    where
        nodrop (PlayerAction Drop _) _ _ _ _ = Just (-5)
        nodrop _ _ _ _ _ = Nothing
        buildcomp (PlayerAction BuildCompany _) _ _ _ _ = Just 5
        buildcomp _ _ _ _ _ = Nothing
        points card e _ pid stt = Just (fromIntegral (getVictoryPoint v + (getFunding f - fromIntegral exchanged) `div` 3))
            where
                exchanged = M.size e
                (f,v) = cardEffectPreview pid card (stt ^. playermap)
        military card _ _ pid stt | poach == 0 = Nothing -- not a military card
                                  | all (< myscore) neighs = Nothing -- I am leader
                                  | all (< (myscore + poach)) neighs = Just 10 -- I become the leader !
                                  | otherwise = Nothing
            where
                poach = card ^. cEffect . folded . _Poaching
                poachingScores = fmap (view (cardEffects . _Poaching)) (stt ^. playermap)
                getPoach p = poachingScores ^. ix p
                myscore = getPoach pid
                neighs = stt ^.. playermap . ix pid . pNeighborhood . both . to getPoach
        withPlay :: (Card -> Exchange -> Maybe SpecialInformation -> PlayerId -> GameState -> Maybe Int) -> PlayerAction -> Exchange -> Maybe SpecialInformation -> PlayerId -> GameState -> Maybe Int
        withPlay f (PlayerAction Play c) = f c
        withPlay _ _ = \_ _ _ _ -> Nothing
        resources card _ _ pid stt | resChoice > 0 = Just (resChoice * 20) -- Always pick multi-resources cards
                                   | all (not . S.null . S.difference resTypes . MS.toSet) available = Just 20 -- pick resources I don't have
                                   | any (not . S.null . S.difference resTypes . MS.toSet) available = Just 5 -- pick resources I only have as a choice
                                   | otherwise = Nothing
            where resTypes = setOf (cEffect . folded . _ProvideResource . _1) card
                  available = (fmap (availableResources OwnRes) (stt ^. playermap . at pid)) ^. folded
                  resChoice = S.size (card ^. cEffect . folded . _ResourceChoice . _1)


bot1 :: (Applicative p, Monad m) => Strategy p m
bot1 = Strategy pd ac
    where
        ac age pid necards stt _ = return $ pure $ snd $ head $ sortOn (negate . fst) $ do
            card <- NE.toList necards
            priofunc <- priority age
            let s = priofunc (PlayerAction Play card) mempty Nothing pid stt
            r <- s ^.. folded
            return (r, card)
        pd age _ pid necards stt =
            let a = sortOn (negate . fst) $ do
                    (act, exch, special) <- NE.toList $ allowableActions age pid necards (stt ^. playermap)
                    priofunc <- priority age
                    let s = priofunc act exch special pid stt
                    r <- s ^.. folded
                    return (r, (act, exch))
            in  return $ pure $ snd $ head a



