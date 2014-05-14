{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Startups.Utils where

import Startups.Base
import Startups.Cards
import Startups.CardList
import Startups.GameTypes

import Control.Lens
import Data.Foldable (Foldable)
import Control.Applicative
import Data.Monoid
import Control.Monad
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

instance (Monoid n, Ord k) => Monoid (AddMap k n) where
    mempty = mempty
    AddMap m1 `mappend` AddMap m2 = AddMap (M.unionWith (<>) m1 m2)

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
        getTargets' (Neighboring n) = getPlayer pid >>= Just . view (neighbor n) >>= getPlayer

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

-- | Compute the money that a card gives to a player. Note how we exploit the fact that ^. behaves like foldMap here.
getCardFunding :: PlayerId -> Card -> M.Map PlayerId PlayerState -> Funding
getCardFunding pid card stt = card ^. cEffect . traverse . _GainFunding . to computeFunding
    where
        computeFunding (n, cond) = countConditionTrigger pid cond stt * n

-- | Compute the victory points a card awards.
getCardVictory :: PlayerId -> Card -> M.Map PlayerId PlayerState ->  [(VictoryType, VictoryPoint)]
getCardVictory pid card stt = card ^.. cEffect . traverse . _AddVictory . to computeVictory
    where
        computeVictory (vtype, vpoints, vcond) = (vtype, countConditionTrigger pid vcond stt * vpoints)

-- | A data structure that represents special capabilities that are used
-- when playing a card.
data SpecialInformation = UseOpportunity
                        deriving Eq

-- | Gets the list of cheap resources, for each neighbor
getCheapExchanges :: PlayerState -> M.Map Neighbor (S.Set Resource)
getCheapExchanges ps = M.fromListWith (<>) $ do
    (resources, neighs) <- ps ^.. cardEffects . _CheapExchange
    n <- S.toList neighs
    return (n, resources)

-- | This function tries to find all possible exchanges that satisfy
-- a given research
-- This function might need refactoring as it is a bit ugly ...
findExchange :: MS.MultiSet Resource -> M.Map Neighbor (S.Set Resource) -> MS.MultiSet Resource -> MS.MultiSet Resource -> [(Exchange, Funding)]
findExchange toAcq cheapExchanges = runSearch (MS.toList toAcq)
    where
        cost v r = if has (ix v . ix r) cheapExchanges then 1 else 2
        runSearch :: [Resource] -> MS.MultiSet Resource -> MS.MultiSet Resource -> [(Exchange, Funding)]
        runSearch [] _ _ = [(mempty, 0)]
        runSearch (t:ts) lp rp =
            let lexchange = if MS.member t lp
                                then map (addExchange NLeft) (runSearch ts (MS.delete t lp) rp)
                                else []
                rexchange = if MS.member t rp
                                then map (addExchange NRight) (runSearch ts lp (MS.delete t rp))
                                else []
                addExchange v (e, m) = (M.insertWith (<>) v (MS.singleton t) e , m + cost v t)
            in  lexchange ++ rexchange

-- | List all the ways a given card can be built. This is the most tricky
-- function.
getCardActions :: Age -> PlayerState -> [MS.MultiSet Resource] -> [MS.MultiSet Resource] -> Card -> [(PlayerAction, Exchange, Maybe SpecialInformation)]
getCardActions age playerstate lplayer rplayer card
    -- We can't build 2 cards with the same name
    | alreadyBuilt ^. contains cardname = []
    -- We can have a card that enable free construction of this card
    | freeConstruction playerstate ^. contains cardname = [build mempty Nothing]
    -- We can have enough resources, but might prefer to use the
    -- opportunity effect if a card costs money
    | any (neededresources `MS.isSubsetOf`) myresources && (neededfunding <= myfunding) = if neededfunding > 0
                                                                                              then build mempty Nothing : opportunity
                                                                                              else [build mempty Nothing]
    -- Otherwise, it's time to check for exchanges, and the opportunity
    -- effect
    | otherwise = map (`build` Nothing) bestExchange ++ opportunity
    where
        -- this is an empty list unless the player has the opportunity
        -- effect ready
        opportunity = [ build mempty (Just UseOpportunity) | has opportunityEffect playerstate ]
        -- this is a traversal that checks if the opportunity effect is
        -- ready
        opportunityEffect = cardEffects . _Opportunity . ix age
        -- Some helpers ...
        build e s = (PlayerAction Play card, e, s)
        cardname = view cName card
        alreadyBuilt = setOf (pCards . traverse . cName) playerstate
        myresources = availableResources OwnRes playerstate
        myfunding = playerstate ^. pFunds
        Cost neededresources neededfunding = card ^. cCost
        -- This is suboptimal : we keep all exchanges that cost the least
        -- amount of money. What would be better would be to also filter
        -- the exchanges that give the same amount of money to the same
        -- neighbors.
        bestExchange = map fst $ filter ((==leastFunding) . snd) checkExchange
        leastFunding = minimum (map snd checkExchange)
        -- This constructs all the possible exchanges, for all combinations
        -- of resources for all players.
        checkExchange :: [(Exchange, Funding)]
        checkExchange = do
            guard (myfunding >= neededfunding)
            curresources <- myresources
            let resourcesToAcquire = neededresources `MS.difference` curresources
            lplayer' <- lplayer
            rplayer' <- rplayer
            (exchange, ecost) <- findExchange resourcesToAcquire (getCheapExchanges playerstate) lplayer' rplayer'
            guard (ecost + neededfunding <= myfunding)
            return (exchange, ecost)

-- | List all possible actions a player can take, given a list of cards
allowableActions :: Age -> PlayerId -> [Card] -> M.Map PlayerId PlayerState -> [(PlayerAction, Exchange, Maybe SpecialInformation)]
allowableActions age pid cards players =
    let playerNeighborInformation = do
            mpstate <- players ^. at pid
            lpstate <- players ^. at (mpstate ^. neighbor NLeft)
            rpstate <- players ^. at (mpstate ^. neighbor NRight)
            return (mpstate, availableResources Exchange lpstate, availableResources Exchange rpstate)
        -- all cards can always be dropped
        dropped = map ( (,mempty,Nothing) . PlayerAction Drop ) cards
    in  case playerNeighborInformation of
            Just (playerstate, lplayer, rplayer) ->
                -- the company stuff, checks if we can build it
                let cstage     = playerstate ^. pCompanyStage
                    comp       = playerstate ^. pCompany
                    nstagecard = getResourceCard comp (succ cstage)
                    maxstage   = getMaxStage comp
                    compaction | cstage == maxstage = []
                               | otherwise = do
                                   (_, exch, si) <- getCardActions age playerstate lplayer rplayer nstagecard
                                   -- you can't build your company using a special ability. This is artificial,
                                   -- this check should be done at the "getCardActions" part.
                                   guard (has _Nothing si)
                                   cardToDrop <- cards
                                   return (PlayerAction BuildCompany cardToDrop, exch, Nothing)
                in concatMap (getCardActions age playerstate lplayer rplayer) cards ++ compaction
            _ -> dropped
