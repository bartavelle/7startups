{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Some pretty printing, with specific constructors for the game
module Startups.PrettyPrint where

import Startups.Base
import Startups.Cards

import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.String
import Control.Monad.Error
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Text as T
import qualified Data.Foldable as F
import Data.List (intersperse)
import Control.Lens
import qualified Text.PrettyPrint.ANSI.Leijen as PP

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
                   | PResearch ResearchType
                   | PCardType CardType
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

instance PrettyE PrettyDoc where
    pe = id
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
instance PrettyE ResearchType where
    pe = PrettyDoc . Seq.singleton . PResearch
instance PrettyE CardType where
    pe = PrettyDoc . Seq.singleton . PCardType

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

cardEffectShort :: Effect -> PrettyDoc
cardEffectShort c = case c of
    ProvideResource r n _   -> "+" <> mconcat (replicate n (pe r))
    ResourceChoice rs _     -> "+" <> sepBy "/" (map pe (F.toList rs))
    CheapExchange rs t      -> "Exch." <+> F.foldMap pe (F.toList rs) <+> F.foldMap pe t
    AddVictory vc v cond    -> "+" <> victory v vc <+> conditionShort cond
    GainFunding m cond      -> "+" <> pe m <+> conditionShort cond
    RnD s                   -> pe s
    Poaching p              -> "+" <> pe p
    ScientificBreakthrough  -> "+" <> sepBy "/" (map pe [Scaling, Programming, CustomSolution])
    Recycling               -> "Play a discarded card"
    Opportunity _           -> "Build for free once/age"
    Efficiency              -> "Play the last card of the age"
    CopyCommunity           -> "Copy a neighbor's community"

conditionShort :: Condition -> PrettyDoc
conditionShort cond = case cond of
    HappensOnce          -> mempty
    PerCard t c          -> sepBy "/" $ F.foldMap pe t : map pe (F.toList c)
    ByPoachingResult t o -> sepBy "/" $ F.foldMap pe t : map pe (F.toList o)
    ByStartupStage t     -> "per stage" <+> brackets (F.foldMap pe t)

cardName :: Card -> PrettyDoc
cardName card = case card of
    Card cn _ _ ct _ _ _ -> withCardColor ct (pe cn)
    CompanyCard c s _ _  -> pe c <+> pe s

shortCard :: Card -> PrettyDoc
shortCard card = cardName card <+> pcost (card ^. cCost) <+> pe (map cardEffectShort (card ^. cEffect))

longCard :: Card -> PrettyDoc
longCard c = shortCard c <> page
                         <> if null grt
                                 then mempty
                                 else mempty <+> "- Free:" <+> pe grt
    where
        grt = c ^.. cFree . traverse . to pe
        page = fromMaybe mempty (c ^? cAge . to pe . to brackets)

instance PP.Pretty PrettyDoc where
    pretty = F.foldMap PP.pretty . getDoc

prettyColor :: PColor -> PP.Doc -> PP.Doc
prettyColor (PColorCard c) = case c of
    BaseResource        -> id
    AdvancedResource    -> id
    Infrastructure      -> PP.dullcyan
    ResearchDevelopment -> PP.green
    Commercial          -> PP.dullyellow
    HeadHunting         -> PP.red
    Community           -> PP.magenta
prettyColor (PColorVictory v) = case v of
    PoachingVictory       -> PP.red
    FundingVictory        -> PP.dullyellow
    CompanyVictory        -> id
    InfrastructureVictory -> PP.dullblue
    RnDVictory            -> PP.dullgreen
    CommercialVictory     -> PP.dullred
    CommunityVictory      -> PP.magenta

instance PP.Pretty PrettyElement where
    pretty e = case e of
        RawText t                  -> PP.string (T.unpack t)
        NewLine                    -> PP.linebreak
        Space                      -> PP.space
        Emph d                     -> PP.bold (PP.pretty d)
        Colorize c d               -> prettyColor c (PP.pretty d)
        Indent n d                 -> PP.indent n (PP.pretty d)
        PDirection Own             -> "⇓"
        PDirection (Neighboring n) -> PP.pretty (PNeighbor n)
        PNeighbor NLeft            -> "◀"
        PNeighbor NRight           -> "▶"
        PFund (Funding 0)          -> mempty
        PFund (Funding n)          -> PP.yellow (PP.pretty (numerical n) <> "$")
        PPoach (Poacher 0)         -> mempty
        PPoach (Poacher p)         -> PP.red $ if p > 5
                                                   then fromString (replicate (fromIntegral p) '⚔')
                                                   else PP.pretty (numerical p) <> "⚔"
        PVictory vp                -> PP.pretty $ numerical vp
        PPlayerCount pc            -> PP.pretty $ numerical pc
        PTurn t                    -> PP.pretty $ numerical t
        PResource Youthfulness     -> PP.cyan "Y"
        PResource Adoption         -> PP.dullwhite  "A"
        PResource Vision           -> PP.magenta    "V"
        PResource Development      -> PP.dullyellow "D"
        PResource Marketing        -> PP.dullgreen  "M"
        PResource Finance          -> PP.dullwhite  "F"
        PResource Operations       -> PP.white      "O"
        PAge Age1                  -> "Ⅰ"
        PAge Age2                  -> "Ⅱ"
        PAge Age3                  -> "Ⅲ"
        PCompanyStage Project      -> "."
        PCompanyStage Stage1       -> "_"
        PCompanyStage Stage2       -> "="
        PCompanyStage Stage3       -> "Δ"
        PCompanyStage Stage4       -> "☥"
        PConflict Defeat           -> PP.red "-1"
        PConflict (Victory Age1)   -> PP.red "+1"
        PConflict (Victory Age2)   -> PP.red "+3"
        PConflict (Victory Age3)   -> PP.red "+5"
        PCardType t                -> prettyColor (PColorCard t) (fromString (show t))
        PResearch r                -> PP.green $ PP.char (head (show r))
        PCompany (CompanyProfile c s) -> PP.string (show c) <> PP.string (show s)
