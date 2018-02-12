{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Startups.Base where

import Startups.Json
import qualified Data.Set as S
import Control.Lens
import Data.Aeson hiding (defaultOptions)
import Data.Semigroup
import Elm.Derive

data Age = Age1
         | Age2
         | Age3
         deriving (Ord,Eq,Enum,Show, Bounded)

data CompanyStage = Project
                  | Stage1
                  | Stage2
                  | Stage3
                  | Stage4
                  deriving (Ord,Eq,Enum,Show, Bounded)

data Company = Facebook     -- ^ Rhodes
             | Twitter      -- ^ Alexandria
             | Apple        -- ^ Arthemis
             | Google       -- ^ Babylon
             | Yahoo        -- ^ Zeus statue
             | Amazon       -- ^ Halicarnasse
             | Microsoft    -- ^ Gizeh
             deriving (Eq, Ord, Enum, Show, Bounded)

data CompanySide = A | B
                 deriving (Eq, Ord, Enum, Show, Bounded)

data CompanyProfile = CompanyProfile !Company !CompanySide
                    deriving (Eq, Ord, Show)

data Resource = Hype        -- ^ Glass
              | Vision      -- ^ Papyrus
              | Adoption    -- ^ Fabric
              | Development -- ^ Clay
              | Operations  -- ^ Stone
              | Marketing   -- ^ Wood
              | Finance     -- ^ Ore
              deriving (Ord, Eq, Show, Enum, Bounded)

baseResources :: S.Set Resource
baseResources = S.fromList [Development, Operations, Marketing, Finance]

advancedResources :: S.Set Resource
advancedResources = S.fromList [Adoption, Vision, Hype]

newtype Poacher = Poacher { getPoacher :: Integer }
    deriving (Ord, Eq, Num, Integral, Real, Enum, Show, FromJSON, ToJSON)

newtype VictoryPoint = VictoryPoint { getVictoryPoint :: Integer }
    deriving (Ord, Eq, Num, Integral, Real, Enum, Show, FromJSON, ToJSON)

newtype Funding = Funding { getFunding :: Integer }
    deriving (Ord, Eq, Num, Integral, Real, Enum, Show, FromJSON, ToJSON)

newtype PlayerCount = PlayerCount { getPlayerCount :: Integer }
    deriving (Ord, Eq, Num, Integral, Real, Enum, Show, FromJSON, ToJSON)

newtype Turn = Turn { getTurn :: Integer }
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

data PoachingOutcome = Defeat
                     | Victory !Age
                     deriving (Eq, Ord, Show)

data VictoryType = PoachingVictory
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
$(deriveBoth baseOptions ''PoachingOutcome)
$(deriveBoth baseOptions ''Age)
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
