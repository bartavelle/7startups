{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Startups.Cards where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified RMultiSet as MS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Data.String
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson hiding (defaultOptions)
import Data.Semigroup
import Elm.Derive
import Data.Char (toLower)

import Startups.Base

data CardType
    = BaseResource            -- ^ The "brown" cards, provide basic resources
    | AdvancedResource        -- ^ The "grey" cards, provide advanced resources
    | Infrastructure          -- ^ The "blue" cards, directly give victory points
    | ResearchDevelopment     -- ^ The "green" cards, the more you have, the more victory points you get
    | Commercial              -- ^ The "gold" cards, mainly get you funding
    | HeadHunting             -- ^ The "red" cards, giving poaching power
    | Community               -- ^ The "purple" cards, giving victory points according to various conditions
    deriving (Eq, Show, Ord, Enum, Bounded)

data Neighbor
    = NLeft
    | NRight
    deriving (Ord, Eq, Show, Enum, Bounded)

data EffectDirection
    = Neighboring !Neighbor
    | Own
    deriving (Ord, Eq, Show)

type Target = S.Set EffectDirection

data Condition
    = HappensOnce
    | PerCard !Target (S.Set CardType)
    | ByPoachingResult !Target (S.Set PoachingOutcome)
    | ByStartupStage !Target
    deriving (Ord, Eq, Show)

neighbors :: Target
neighbors = S.fromList [Neighboring NLeft, Neighboring NRight]

myself :: Target
myself = S.singleton Own

everyone :: Target
everyone = neighbors <> myself

data Sharing = Shared | Kept
             deriving (Ord, Eq, Show, Enum, Bounded)

data ResearchType = Scaling
                  | Programming
                  | CustomSolution
                  deriving (Ord, Eq, Show, Enum, Bounded)

data Effect = ProvideResource !Resource !Int !Sharing
            | ResourceChoice (S.Set Resource) !Sharing
            | CheapExchange (S.Set Resource) (S.Set Neighbor)
            | AddVictory !VictoryType !VictoryPoint !Condition
            | GainFunding !Funding !Condition
            | RnD !ResearchType
            | Poaching !Poacher
            | ScientificBreakthrough -- gives any science type
            | Recycling -- play a card in the discard pile
            | Opportunity (S.Set Age) -- build for free once per age
            | Efficiency -- play the last card
            | CopyCommunity
            deriving (Ord, Eq, Show)

data Cost = Cost MS.ResourceSet !Funding
          deriving (Ord, Eq, Show)

instance Semigroup Cost where
    Cost r1 f1 <> Cost r2 f2 = Cost (r1 <> r2) (f1 + f2)

instance Monoid Cost where
    mappend = (<>)
    mempty = Cost mempty 0

instance IsString Cost where
    fromString = F.foldMap toCost
        where
            toCost 'H' = Cost (MS.singleton Hype) 0
            toCost 'V' = Cost (MS.singleton Vision) 0
            toCost 'A' = Cost (MS.singleton Adoption) 0
            toCost 'D' = Cost (MS.singleton Development) 0
            toCost 'O' = Cost (MS.singleton Operations) 0
            toCost 'M' = Cost (MS.singleton Marketing) 0
            toCost 'F' = Cost (MS.singleton Finance) 0
            toCost '$' = Cost mempty 1
            toCost _   = error "Invalid cost string"

data Card = Card { _cName       :: !T.Text
                 , _cMinplayers :: !PlayerCount
                 , _cAge        :: !Age
                 , _cType       :: !CardType
                 , _cCost       :: !Cost
                 , _cFree       :: [T.Text]
                 , _cEffect     :: [Effect]
                 }
          | CompanyCard { _cCompany :: !CompanyProfile
                        , _cStage   :: !CompanyStage
                        , _cCost    :: !Cost
                        , _cEffect  :: [Effect]
                        }
          deriving (Ord,Eq,Show)

newtype Exchange = RExchange { getExchange :: M.Map Neighbor MS.ResourceSet }
                   deriving (Show, Eq)

instance Semigroup Exchange where
    RExchange a <> RExchange b = RExchange (M.unionWith (<>) a b)

instance Monoid Exchange where
    mappend = (<>)
    mempty = RExchange mempty

instance ToJSON Exchange where
    toJSON = toJSON . map (_2 %~ MS.toList) . itoList . getExchange

instance FromJSON Exchange where
    parseJSON = fmap (RExchange . M.fromList) . (parseJSON >=> mapM parsePair)
        where
            parsePair (n,rs) = (,) <$> parseJSON n <*> parseMultiset rs
            parseMultiset = fmap MS.fromList . parseJSON

makePrisms ''CardType
makePrisms ''Effect
makeLenses ''Card

instance ToJSON Cost where
    toJSON (Cost c f) = object [ "resources" .= c
                               , "funding"   .= f
                               ]

instance FromJSON Cost where
    parseJSON = withObject "cost" $ \o -> Cost <$> o .: "resources"
                                               <*> o .: "funding"

$(deriveElmDef defaultOptions ''Exchange)
$(deriveBoth defaultOptions ''CardType)
$(deriveBoth defaultOptions ''Effect)
$(deriveBoth defaultOptions ''Condition)
$(deriveBoth defaultOptions ''Neighbor)
$(deriveBoth defaultOptions ''EffectDirection)
$(deriveBoth defaultOptions ''Sharing)
$(deriveBoth defaultOptions ''ResearchType)
$(deriveBoth defaultOptions { fieldLabelModifier = map toLower . drop 2 } ''Card)
