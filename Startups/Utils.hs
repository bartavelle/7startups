{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Startups.Utils where

import Startups.Base
import Startups.Cards
import Startups.GameTypes

import Control.Lens
import Data.Foldable (Foldable)
import Control.Applicative
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Set.Lens
import qualified Data.Map.Strict as M
import qualified Data.MultiSet as MS

-- | We will use this type to define a custom monoid instance for Map k n,
-- when n is numerical. This will be used to simplify some expressions. It
-- should be usable like a standard Map, so we will derive a few instances.
newtype AddMap k n = AddMap { getAddMap :: M.Map k n }
                     deriving (Eq, Ord, Functor, Foldable, Traversable)

instance FunctorWithIndex k (AddMap k)
instance FoldableWithIndex k (AddMap k)
instance TraversableWithIndex k (AddMap k) where
    itraverse f (AddMap m) = AddMap <$> M.traverseWithKey f m

type instance IxValue (AddMap k a) = a
type instance Index (AddMap k a) = k

instance Ord k => Ixed (AddMap k a) where
    ix k f (AddMap m) = case M.lookup k m of
        Just v  -> f v <&> \v' -> AddMap $ M.insert k v' m
        Nothing -> pure (AddMap m)
    {-# INLINE ix #-}

instance Ord k => At (AddMap k a) where
    at k f (AddMap m) = f mv <&> \r -> case r of
        Nothing -> AddMap $ maybe m (const (M.delete k m)) mv
        Just v' -> AddMap $ M.insert k v' m
        where mv = M.lookup k m
    {-# INLINE at #-}

instance (Num n, Ord k) => Monoid (AddMap k n) where
    mempty = mempty
    AddMap m1 `mappend` AddMap m2 = AddMap (M.unionWith (+) m1 m2)

data ResourceQueryType = Exchange | OwnRes
                       deriving Eq

-- | Gets all possible resource combinations that are available. The
-- results depend on whether the resource is shareable.
availableResources :: ResourceQueryType -> PlayerState -> [MS.MultiSet Resource]
availableResources qt p =
    let getR (ProvideResource r n t) = [(r,n) | isApplicable t]
        getR (ResourceChoice rs t) = if isApplicable t
                                         then rs ^.. folded . to (,1)
                                         else []
        getR _ = []
        isApplicable t = t == Shared || qt == OwnRes
        effects = p ^.. cardEffects . to getR . filtered (not . null)
        getCombination :: [[(Resource, Int)]] -> [MS.MultiSet Resource]
        getCombination [] = [mempty]
        getCombination (x:xs) = do
            (cv, cn) <- x
            rst <- getCombination xs
            return (MS.insertMany cv cn rst)

    in  getCombination effects

-- | Computes if it is possible to get a cheap exchange rate for a given
-- resource.
getExchangeCost :: PlayerId -> Neighbor -> M.Map PlayerId PlayerState -> Resource -> Funding
getExchangeCost pid neigh playrmap res =
    let cheapResources = playrmap ^. ix pid
                                   . cardEffects
                                   . _CheapExchange
                                   . filtered (\(_,ns) -> ns ^. contains neigh)
                                   . _1
    in  if cheapResources ^. contains res
            then 1
            else 2

-- | Checks whether a given player, with extra resources (coming from an
-- exchange), can afford a given card, resource-wise. It doesn't check the
-- funds.
isAffordable :: PlayerState -> MS.MultiSet Resource -> Card -> Bool
isAffordable playerState extraResources card =
    let allresources = map (<> extraResources) (availableResources OwnRes playerState)
        Cost rescost _ = card ^. cCost
    in  any (rescost `MS.isSubsetOf`) allresources

-- | This retrieves the player states corresponding to the target
-- definition. It silently discards targets that it can find (because the
-- state is corrupt, which should not happen, as we have been really
-- careful :) ).
getTargets :: PlayerId -> Target -> M.Map PlayerId PlayerState -> [PlayerState]
getTargets pid targets stt = targets ^.. folded . to getTargets' . traverse
    where
        getPlayer p = stt ^. at p
        getTargets' Own = getPlayer pid
        getTargets' (Neighboring n) = getPlayer pid >>= view (pNeighborhood . at n) >>= getPlayer

-- | Given a condition, counts how often it hits. It can return an
-- arbitrary Num because we will multiply this results with Funding or
-- VictoryPoint.
countConditionTrigger :: Num n => PlayerId -> Condition -> M.Map PlayerId PlayerState -> n
countConditionTrigger _ HappensOnce _ = 1
countConditionTrigger pid (ByPoachingResult t po) stt =
    let players = getTargets pid t stt
        poachingTokens = players ^.. traverse . pPoachingResults . traverse . filtered (\e -> po ^. contains e)
    in  fromIntegral (length poachingTokens)
countConditionTrigger pid (PerCard t ct) stt =
    let players = getTargets pid t stt
        matchingCards = players ^.. traverse . pCards . traverse . cType . filtered (\c -> ct ^. contains c)
    in  fromIntegral (length matchingCards)
countConditionTrigger pid (ByStartupStage t) stt =
    let players = getTargets pid t stt
        stageValues = players ^.. traverse . pCompanyStage . to (fromIntegral . fromEnum)
    in  sum stageValues

-- | The list of cards a player can build for free.
freeConstruction :: PlayerState -> S.Set T.Text
freeConstruction = setOf (pCards . traverse . cFree  . traverse)

-- | The science score computation.
scienceScore :: [ResearchType] -> Int -> VictoryPoint
scienceScore rt 0 =
    let eachtypes = map (\t -> length (filter (== t) rt)) [Scaling, Programming, CustomSolution]
    in  fromIntegral $ sum (map (\x -> x * x) eachtypes) + minimum eachtypes * 7
scienceScore rt jokers = maximum [ scienceScore (t : rt) (jokers -1) | t <- [Scaling, Programming, CustomSolution] ]
