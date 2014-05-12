{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Some pretty printing, with specific constructors for the game
module Startups.PrettyPrint where

import Startups.Base
import Startups.Cards
import Startups.GameTypes

import Data.Monoid
import Data.String
import Control.Monad.Error
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Text as T
import qualified Data.Foldable as F
import Data.List (intersperse)
import Control.Lens

newtype PrettyDoc = PrettyDoc { getDoc :: Seq PrettyElement }
                  deriving (Eq, Monoid)

data PrettyElement = RawText T.Text
                   | NewLine
                   | Space
                   | Emph PrettyDoc
                   | Colorize PColor PrettyDoc
                   | Indent Int PrettyDoc
                   | PDirection EffectDirection
                   | PNeighbor Neighbor
                   | PFund Funding
                   | PPoach Poacher
                   | PVictory VictoryPoint
                   | PPlayerCount PlayerCount
                   | PTurn Turn
                   | PResource Resource
                   | PAge Age
                   | PCompanyStage CompanyStage
                   | PConflict PoachingOutcome
                   | PCompany CompanyProfile
                   | PPlayer PlayerId
                   deriving Eq

data PColor = PColorCard CardType
            | PColorVictory VictoryType
            deriving Eq

instance IsString PrettyDoc where
    fromString = PrettyDoc . Seq.singleton . RawText . T.pack

instance Error PrettyDoc where
    noMsg = mempty
    strMsg = fromString

class PrettyE a where
    pe :: a -> PrettyDoc

instance PrettyE T.Text where
    pe = PrettyDoc . Seq.singleton . RawText
instance PrettyE EffectDirection where
    pe = PrettyDoc . Seq.singleton . PDirection
instance PrettyE Neighbor where
    pe = PrettyDoc . Seq.singleton . PNeighbor
instance PrettyE Funding where
    pe = PrettyDoc . Seq.singleton . PFund
instance PrettyE Poacher where
    pe = PrettyDoc . Seq.singleton . PPoach
instance PrettyE VictoryPoint where
    pe = PrettyDoc . Seq.singleton . PVictory
instance PrettyE PlayerCount where
    pe = PrettyDoc . Seq.singleton . PPlayerCount
instance PrettyE Turn where
    pe = PrettyDoc . Seq.singleton . PTurn
instance PrettyE Resource where
    pe = PrettyDoc . Seq.singleton . PResource
instance PrettyE Age where
    pe = PrettyDoc . Seq.singleton . PAge
instance PrettyE CompanyStage where
    pe = PrettyDoc . Seq.singleton . PCompanyStage
instance PrettyE PoachingOutcome where
    pe = PrettyDoc . Seq.singleton . PConflict
instance PrettyE CompanyProfile where
    pe = PrettyDoc . Seq.singleton . PCompany

instance (PrettyE a, F.Foldable f) => PrettyE (f a) where
    pe l = brackets $ sepBy (pchar ',') (map pe (F.toList l))

victory :: VictoryPoint -> VictoryType -> PrettyDoc
victory v vc = withVictoryColor vc (pe v)

brackets :: PrettyDoc -> PrettyDoc
brackets e = pchar '[' <> e <> pchar ']'

space :: PrettyDoc
space = PrettyDoc (Seq.singleton Space)

sepBy :: F.Foldable f => PrettyDoc -> f PrettyDoc -> PrettyDoc
sepBy sep = mconcat . intersperse sep . F.toList

pchar :: Char -> PrettyDoc
pchar = pe . T.singleton

withCardColor :: CardType -> PrettyDoc -> PrettyDoc
withCardColor c = PrettyDoc . Seq.singleton . Colorize (PColorCard c)

withVictoryColor :: VictoryType -> PrettyDoc -> PrettyDoc
withVictoryColor v = PrettyDoc . Seq.singleton . Colorize (PColorVictory v)

showPlayerId :: PlayerId -> PrettyDoc
showPlayerId = PrettyDoc . Seq.singleton . PPlayer

numerical :: Integral n => n -> PrettyDoc
numerical = fromString . (show :: Integer -> String) . fromIntegral

emph :: PrettyDoc -> PrettyDoc
emph = PrettyDoc . Seq.singleton . Emph

pcost :: Cost -> PrettyDoc
pcost (Cost r m) = F.foldMap pe r <> pe m

indent :: Int -> PrettyDoc -> PrettyDoc
indent d = PrettyDoc . Seq.singleton . Indent d

(<+>) :: PrettyDoc -> PrettyDoc -> PrettyDoc
a <+> b = a <> PrettyDoc (Seq.singleton Space) <> b

(</>) :: PrettyDoc -> PrettyDoc -> PrettyDoc
a </> b = a <> newline <> b

newline :: PrettyDoc
newline = PrettyDoc (Seq.singleton NewLine)

vcat :: [PrettyDoc] -> PrettyDoc
vcat = mconcat . intersperse newline


