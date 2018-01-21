{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Some common functions, that work with most text based backends
module Backends.Common where

import Startups.Base
import Startups.Cards
import Startups.GameTypes
import Startups.Game
import Startups.PrettyPrint
import Startups.Utils
import Startups.CardList
import Backends.Pure
import Strategies.Random

import qualified Data.Map.Strict as M
import Control.Lens
import Data.Monoid
import qualified Data.Foldable as F
import System.Random (mkStdGen)
import Data.List (sort)
import Data.Tuple (swap)
import Data.List.NonEmpty (NonEmpty)

cardpreview :: PlayerId -> M.Map PlayerId PlayerState -> Card -> PrettyDoc
cardpreview pid pmap card =
    let (f, vp) = cardEffectPreview pid card pmap
        pvp = victory vp CompanyVictory
        pf = pe f
    in  case (f, vp) of
            (0, 0) -> mempty
            (0, _) -> brackets pvp
            (_, 0) -> brackets pf
            _      -> brackets (pf <> "/" <> pvp)

data ShowMode = Private | Public
              deriving Eq

playerActionDesc :: ShowMode -> PlayerId -> M.Map PlayerId PlayerState -> (PlayerAction, Exchange, Maybe SpecialInformation) -> PrettyDoc
playerActionDesc showmode pid pmap (PlayerAction a card, exch, si) = actiondesc a <+> secret (cardName card)
                                                                                  <+> exchdesc (getExchange exch)
                                                                                  <> sidesc si
    where
        secret n = if showmode == Private || a == Play
                       then n
                       else mempty
        actiondesc Play = withCardColor Infrastructure "Play"
        actiondesc Drop = withCardColor HeadHunting    "Drop"
        actiondesc BuildCompany = withCardColor Community "Build a company stage" <+> secret "using"
        nn n = pmap ^. ix pid . neighbor n
        exchdesc = sepBy ", " . map (uncurry exchdesc') . itoList
        exchdesc' neigh resources = "exch." <+> F.foldMap pe resources
                                            <+> "with"
                                            <+> showPlayerId (nn neigh)
                                            <+> "for"
                                            <+> pe (F.foldMap (getExchangeCost pid neigh pmap) resources)
        sidesc (Just UseOpportunity) = " using the opportunity capability"
        sidesc Nothing = mempty


playerActionsDialog :: PlayerId -> M.Map PlayerId PlayerState -> NonEmpty Card -> NonEmpty (PlayerAction, Exchange, Maybe SpecialInformation) -> PrettyDoc
playerActionsDialog pid pmap clist actions =
        "Your hand:"
        </> vcat [ longCard card <+> cardpreview pid pmap card | card <- F.toList clist ]
        </> "What are you going to play ?"
        </> vcat [ numerical (n :: Int) <> " - " <> playerActionDesc Private pid pmap pa | (n,pa) <- zip [0..] (F.toList actions) ]

cardChoiceDialog :: PlayerId -> M.Map PlayerId PlayerState -> [Card] -> PrettyDoc
cardChoiceDialog pid pmap cards = vcat [ numerical (n :: Int) <> " - " <> desc c | (n,c) <- zip [0..] cards ]
    where
        desc c = longCard c <+> cardpreview pid pmap c

playerDescShort :: PlayerState -> PrettyDoc
playerDescShort p@(PlayerState c cs _ f _ _) = brackets (pe c <> "-" <> pe cs)
                                                <+> brackets (pe NLeft <> nn NLeft <+> nn NRight <> pe NRight)
                                                <+> pe f
                                                <+> availableresources
                                                <+> poach
                                                <+> science
    where
        nn ne = pe (view (neighbor ne) p)
        availableresources = simpleres <+> multires
        simpleres = F.foldMap pe $ sort $ concatMap (\(r,n,_) -> replicate n r) $ p ^.. cardEffects . _ProvideResource
        multires = F.foldMap (brackets . F.foldMap pe . F.toList) $ p ^.. cardEffects . _ResourceChoice . _1
        poach = pe $ p ^. cardEffects . _Poaching
        science = F.foldMap pe $ sort $ p ^.. cardEffects . _RnD

quicksituation :: Age -> Turn -> M.Map PlayerId PlayerState -> PrettyDoc
quicksituation age turn stt = hdr </> situationRecap stt
    where
        hdr = "Age" <+> pe age <+> ", turn" <+> numerical turn

displayActions :: M.Map PlayerId PlayerState -> M.Map PlayerId (PlayerAction, Exchange, Maybe SpecialInformation) -> PrettyDoc
displayActions pmap actionmap = vcat [ showPlayerId pid <+> playerActionDesc Public pid pmap fullAction | (pid, fullAction) <- M.toList actionmap ]

displayVictory :: M.Map PlayerId (M.Map VictoryType VictoryPoint) -> PrettyDoc
displayVictory = vcat . map displayLine . itoList
    where
        displayLine (pid, vmap) = showPlayerId pid <+> victory (vmap ^. traverse) CompanyVictory
                                                   <+> foldPretty (map (uncurry victory . swap) (itoList vmap))

displayCommunication :: Communication -> PrettyDoc
displayCommunication (RawMessage d) = d
displayCommunication (ActionRecapMsg (ActionRecap age turn pmap mp))
  =   displayActions pmap mp
  </> quicksituation age turn pmap

playerStartup :: CompanyProfile -> CompanyStage -> PrettyDoc
playerStartup cp cs = vcat $ map genStage [Project .. getMaxStage cp]
    where
        genStage s = hdr s <+> pe s <+> shortCard (getResourceCard cp s)
        hdr s = if s == cs
                    then "*"
                    else " "

vicmap :: M.Map PlayerId PlayerState -> M.Map PlayerId VictoryPoint
vicmap stt = victorydetails & traverse %~ (mconcat . M.elems)
    where
        victorydetails = runPure stdGenStateStrat (mkStdGen 0) (GameState stt [] (mkStdGen 0)) victoryPoints ^. _2 . _Right

-- | One line recap for a player
playerRecap :: PlayerId -> PlayerState -> VictoryPoint -> PrettyDoc
playerRecap n ps vp = showPlayerId n <+> brackets (victory vp CompanyVictory <> "♚") <+> playerDescShort ps

situationRecap :: M.Map PlayerId PlayerState -> PrettyDoc
situationRecap stt = vcat $ map (\(n,ps) -> playerRecap n ps (vm ^. ix n)) (M.toList stt)
    where
        vm = vicmap stt

detailedSituationRecap :: M.Map PlayerId PlayerState -> PrettyDoc
detailedSituationRecap stt = vcat $ M.elems $ M.mapWithKey precap stt
    where
        precap n ps = playerRecap n ps (vm ^. ix n) </> indent 4 (vcat (map shortCard (ps ^. pCards)))
        vm = vicmap stt
