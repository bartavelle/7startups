{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Startups.Base where

import qualified Data.Set as S
import Control.Lens

data Age = Age1
         | Age2
         | Age3
         deriving (Ord,Eq,Enum,Show)

data CompanyStage = Project
                  | Stage1
                  | Stage2
                  | Stage3
                  | Stage4
                  deriving (Ord,Eq,Enum)

data Company = Facebook     -- ^ Rhodes
             | Twitter      -- ^ Alexandria
             | Apple        -- ^ Arthemis
             | Google       -- ^ Babylon
             | Instagram    -- ^ Zeus statue
             | Amazon       -- ^ Halicarnasse
             | Microsoft    -- ^ Gizeh
             deriving (Eq, Ord, Enum, Show)

data CompanySide = A | B
                 deriving (Eq, Ord, Enum)

data CompanyProfile = CompanyProfile Company CompanySide
                    deriving (Eq, Ord)

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
    deriving (Ord, Eq, Num, Integral, Real, Enum, Show)

newtype VictoryPoint = VictoryPoint { getVictory :: Integer }
    deriving (Ord, Eq, Num, Integral, Real, Enum, Show)

newtype Funding = Funding { getFunding :: Integer }
    deriving (Ord, Eq, Num, Integral, Real, Enum, Show)

newtype PlayerCount = PlayerCount { getPlayerCount :: Integer }
    deriving (Ord, Eq, Num, Integral, Real, Enum, Show)

data PoachingOutcome = Defeat
                     | Victory Age
                     deriving (Eq, Ord, Show)

makePrisms ''Age
makePrisms ''PoachingOutcome
