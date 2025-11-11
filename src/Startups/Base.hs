{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Startups.Base where

import Control.Lens
import Data.Aeson hiding (defaultOptions)
import qualified Data.Set as S
import Elm.Derive
import Startups.Json

data Age
  = Age1
  | Age2
  | Age3
  deriving (Ord, Eq, Enum, Show, Bounded)

data CompanyStage
  = Project
  | Stage1
  | Stage2
  | Stage3
  | Stage4
  deriving (Ord, Eq, Enum, Show, Bounded)

data Company
  = -- | Rhodes
    Facebook
  | -- | Alexandria
    Twitter
  | -- | Arthemis
    Apple
  | -- | Babylon
    Google
  | -- | Zeus statue
    Yahoo
  | -- | Halicarnasse
    Amazon
  | -- | Gizeh
    Microsoft
  deriving (Eq, Ord, Enum, Show, Bounded)

data CompanySide = A | B
  deriving (Eq, Ord, Enum, Show, Bounded)

data CompanyProfile = CompanyProfile !Company !CompanySide
  deriving (Eq, Ord, Show)

data Resource
  = -- | Glass
    Hype
  | -- | Papyrus
    Vision
  | -- | Fabric
    Adoption
  | -- | Clay
    Development
  | -- | Stone
    Operations
  | -- | Wood
    Marketing
  | -- | Ore
    Finance
  deriving (Ord, Eq, Show, Enum, Bounded)

baseResources :: S.Set Resource
baseResources = S.fromList [Development, Operations, Marketing, Finance]

advancedResources :: S.Set Resource
advancedResources = S.fromList [Adoption, Vision, Hype]

newtype Poacher = Poacher {getPoacher :: Integer}
  deriving (Ord, Eq, Num, Integral, Real, Enum, Show, FromJSON, ToJSON)

newtype VictoryPoint = VictoryPoint {getVictoryPoint :: Integer}
  deriving (Ord, Eq, Num, Integral, Real, Enum, Show, FromJSON, ToJSON)

newtype Funding = Funding {getFunding :: Integer}
  deriving (Ord, Eq, Num, Integral, Real, Enum, Show, FromJSON, ToJSON)

newtype PlayerCount = PlayerCount {getPlayerCount :: Integer}
  deriving (Ord, Eq, Num, Integral, Real, Enum, Show, FromJSON, ToJSON)

newtype Turn = Turn {getTurn :: Integer}
  deriving (Ord, Eq, Num, Integral, Real, Enum, Show, FromJSON, ToJSON)

instance Semigroup Poacher where
  Poacher a <> Poacher b = Poacher (a + b)

instance Semigroup VictoryPoint where
  VictoryPoint a <> VictoryPoint b = VictoryPoint (a + b)

instance Semigroup Funding where
  Funding a <> Funding b = Funding (a + b)

instance Monoid Poacher where
  mappend = (<>)
  mempty = 0

instance Monoid VictoryPoint where
  mappend = (<>)
  mempty = 0

instance Monoid Funding where
  mappend = (<>)
  mempty = 0

data PoachingOutcome
  = Defeat
  | Victory !Age
  deriving (Eq, Ord, Show)

data VictoryType
  = PoachingVictory
  | FundingVictory
  | CompanyVictory
  | InfrastructureVictory
  | RnDVictory
  | CommercialVictory
  | CommunityVictory
  deriving (Ord, Eq, Show, Enum, Bounded)

makePrisms ''Age
makePrisms ''PoachingOutcome

$(deriveBoth baseOptions ''VictoryType)
$(deriveBoth baseOptions ''Age)
$(deriveBoth baseOptions ''PoachingOutcome)
$(deriveBoth baseOptions ''Company)
$(deriveBoth baseOptions ''CompanyStage)
$(deriveBoth baseOptions ''CompanySide)
$(deriveBoth baseOptions ''CompanyProfile)
$(deriveBoth baseOptions ''Resource)
$(deriveElmDef baseOptions ''Poacher)
$(deriveElmDef baseOptions ''VictoryPoint)
$(deriveElmDef baseOptions ''Funding)
$(deriveElmDef baseOptions ''PlayerCount)
$(deriveElmDef baseOptions ''Turn)
