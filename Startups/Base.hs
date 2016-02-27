{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Startups.Base where

import qualified Data.Set as S
import Control.Lens
import Data.Monoid
import Data.Aeson
import Data.Aeson.TH

data Age = Age1
         | Age2
         | Age3
         deriving (Ord,Eq,Enum,Show)

data CompanyStage = Project
                  | Stage1
                  | Stage2
                  | Stage3
                  | Stage4
                  deriving (Ord,Eq,Enum,Show)

data Company = Facebook     -- ^ Rhodes
             | Twitter      -- ^ Alexandria
             | Apple        -- ^ Arthemis
             | Google       -- ^ Babylon
             | Yahoo        -- ^ Zeus statue
             | Amazon       -- ^ Halicarnasse
             | Microsoft    -- ^ Gizeh
             deriving (Eq, Ord, Enum, Show)

data CompanySide = A | B
                 deriving (Eq, Ord, Enum, Show)

data CompanyProfile = CompanyProfile Company CompanySide
                    deriving (Eq, Ord, Show)

data Resource = Youthfulness -- ^ Glass
              | Vision       -- ^ Papyrus
              | Adoption     -- ^ Fabric
              | Development  -- ^ Clay
              | Operations   -- ^ Stone
              | Marketing    -- ^ Wood
              | Finance      -- ^ Ore
              deriving (Ord, Eq, Show)

baseResources :: S.Set Resource
baseResources = S.fromList [Development, Operations, Marketing, Finance]

advancedResources :: S.Set Resource
advancedResources = S.fromList [Adoption, Vision, Youthfulness]

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

instance Monoid Poacher where
    mempty = 0
    Poacher a `mappend` Poacher b = Poacher (a + b)
instance Monoid VictoryPoint where
    mempty = 0
    VictoryPoint a `mappend` VictoryPoint b = VictoryPoint (a + b)
instance Monoid Funding where
    mempty = 0
    Funding a `mappend` Funding b = Funding (a + b)

data PoachingOutcome = Defeat
                     | Victory Age
                     deriving (Eq, Ord, Show)

data VictoryType = PoachingVictory
                 | FundingVictory
                 | CompanyVictory
                 | InfrastructureVictory
                 | RnDVictory
                 | CommercialVictory
                 | CommunityVictory
                 deriving (Ord, Eq, Show)

makePrisms ''Age
makePrisms ''PoachingOutcome

$(deriveJSON defaultOptions ''VictoryType)
$(deriveJSON defaultOptions ''PoachingOutcome)
$(deriveJSON defaultOptions ''Age)
$(deriveJSON defaultOptions ''Company)
$(deriveJSON defaultOptions ''CompanyStage)
$(deriveJSON defaultOptions ''CompanySide)
$(deriveJSON defaultOptions ''CompanyProfile)
$(deriveJSON defaultOptions ''Resource)
