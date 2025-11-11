{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Some pretty printing, with specific constructors for the game
module Startups.PrettyPrint where

import Control.Lens
import qualified Data.Foldable as F
import Data.List (intersperse)
import Data.String
import qualified Data.Text as T
import Elm.Derive
import qualified Prettyprinter as PP
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, color, colorDull)
import qualified RMultiSet as MS
import Startups.Base
import Startups.Cards

type Doc = PP.Doc AnsiStyle

data PrettyDoc
  = RawText T.Text
  | NewLine
  | Space
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
  | PEmpty
  | Emph PrettyDoc
  | Colorize PColor PrettyDoc
  | Indent Int PrettyDoc
  | PCat PrettyDoc PrettyDoc
  deriving (Eq, Show)

-- | This little helper dropw the newline keyword for backends that don't
-- support multiline docs.
splitLines :: PrettyDoc -> [PrettyDoc]
splitLines = map simplify . splitLines' mempty
  where
    -- these rules should be enough
    simplify (PCat PEmpty PEmpty) = PEmpty
    simplify (PCat a PEmpty) = simplify a
    simplify (PCat PEmpty b) = simplify b
    simplify (PCat a b) = PCat (simplify a) (simplify b)
    simplify x = x
    splitLines' :: PrettyDoc -> PrettyDoc -> [PrettyDoc]
    splitLines' acc (PCat NewLine b) = acc : splitLines b
    splitLines' acc (PCat a NewLine) = map (PCat acc) (splitLines a)
    splitLines' acc (PCat dx dy) =
      case (splitLines dx, splitLines dy) of
        ([], []) -> [acc]
        ([], y : ys) -> PCat acc y : ys
        (x : xs, []) -> PCat acc x : xs
        (x : xs, y : ys) ->
          if null xs
            then PCat acc (PCat x y) : ys
            else PCat acc x : init xs ++ [PCat (last xs) y] ++ ys
    splitLines' acc (Emph d) = map (PCat acc . Emph) (splitLines d)
    splitLines' acc (Colorize c d) = map (PCat acc . Colorize c) (splitLines d)
    splitLines' acc (Indent n d) = map (PCat acc . Indent n) (splitLines d)
    splitLines' acc NewLine = [acc]
    splitLines' acc x = [PCat acc x]

instance Semigroup PrettyDoc where
  (<>) = PCat

instance Monoid PrettyDoc where
  mappend = (<>)
  mempty = PEmpty

data PColor
  = PColorCard CardType
  | PColorVictory VictoryType
  deriving (Eq, Show)

instance IsString PrettyDoc where
  fromString = RawText . T.pack

class PrettyE a where
  pe :: a -> PrettyDoc

instance PrettyE PrettyDoc where
  pe = id

instance PrettyE T.Text where
  pe = RawText

instance PrettyE EffectDirection where
  pe = PDirection

instance PrettyE Neighbor where
  pe = PNeighbor

instance PrettyE Funding where
  pe = PFund

instance PrettyE Poacher where
  pe = PPoach

instance PrettyE VictoryPoint where
  pe = PVictory

instance PrettyE PlayerCount where
  pe = PPlayerCount

instance PrettyE Turn where
  pe = PTurn

instance PrettyE Resource where
  pe = PResource

instance PrettyE Age where
  pe = PAge

instance PrettyE CompanyStage where
  pe = PCompanyStage

instance PrettyE PoachingOutcome where
  pe = PConflict

instance PrettyE CompanyProfile where
  pe = PCompany

instance PrettyE ResearchType where
  pe = PResearch

instance PrettyE CardType where
  pe = PCardType

foldPretty :: (PrettyE a, F.Foldable f) => f a -> PrettyDoc
foldPretty l = brackets $ sepBy (pchar ',') (map pe (F.toList l))

victory :: VictoryPoint -> VictoryType -> PrettyDoc
victory v vc = withVictoryColor vc (pe v)

brackets :: PrettyDoc -> PrettyDoc
brackets e = pchar '[' <> e <> pchar ']'

space :: PrettyDoc
space = Space

sepBy :: (F.Foldable f) => PrettyDoc -> f PrettyDoc -> PrettyDoc
sepBy sep = mconcat . intersperse sep . F.toList

pchar :: Char -> PrettyDoc
pchar = pe . T.singleton

withCardColor :: CardType -> PrettyDoc -> PrettyDoc
withCardColor c = Colorize (PColorCard c)

withVictoryColor :: VictoryType -> PrettyDoc -> PrettyDoc
withVictoryColor v = Colorize (PColorVictory v)

numerical :: (Integral n) => n -> PrettyDoc
numerical = fromString . (show :: Integer -> String) . fromIntegral

emph :: PrettyDoc -> PrettyDoc
emph = Emph

pcost :: Cost -> PrettyDoc
pcost (Cost r m) = MS.foldMap pe r <> pe m

indent :: Int -> PrettyDoc -> PrettyDoc
indent = Indent

(<+>) :: PrettyDoc -> PrettyDoc -> PrettyDoc
a <+> b = a <> space <> b

(</>) :: PrettyDoc -> PrettyDoc -> PrettyDoc
a </> b = a <> newline <> b

newline :: PrettyDoc
newline = NewLine

vcat :: [PrettyDoc] -> PrettyDoc
vcat = mconcat . intersperse newline

cardEffectShort :: Effect -> PrettyDoc
cardEffectShort c = case c of
  ProvideResource r n _ -> "+" <> mconcat (replicate n (pe r))
  ResourceChoice rs _ -> "+" <> sepBy "/" (map pe (F.toList rs))
  CheapExchange rs t -> "Exch." <+> F.foldMap pe (F.toList rs) <+> F.foldMap pe t
  AddVictory vc v cond -> "+" <> victory v vc <+> conditionShort cond
  GainFunding m cond -> "+" <> pe m <+> conditionShort cond
  RnD s -> pe s
  Poaching p -> "+" <> pe p
  ScientificBreakthrough -> "+" <> sepBy "/" (map pe [Scaling, Programming, CustomSolution])
  Recycling -> "Play a discarded card"
  Opportunity _ -> "Build for free once/age"
  Efficiency -> "Play the last card of the age"
  CopyCommunity -> "Copy a neighbor's community"

conditionShort :: Condition -> PrettyDoc
conditionShort cond = case cond of
  HappensOnce -> mempty
  PerCard t c -> sepBy "/" $ F.foldMap pe t : map pe (F.toList c)
  ByPoachingResult t o -> sepBy "/" $ F.foldMap pe t : map pe (F.toList o)
  ByStartupStage t -> "per stage" <+> brackets (F.foldMap pe t)

cardName :: Card -> PrettyDoc
cardName card = case card of
  Card cn _ _ ct _ _ _ -> withCardColor ct (pe cn)
  CompanyCard c s _ _ -> pe c <+> pe s

shortCard :: Card -> PrettyDoc
shortCard card = cardName card <+> pcost (card ^. cCost) <+> foldPretty (map cardEffectShort (card ^. cEffect))

longCard :: Card -> PrettyDoc
longCard c =
  shortCard c
    <> if null grt
      then mempty
      else mempty <+> "- Free:" <+> foldPretty grt
  where
    grt = c ^.. cFree . traverse . to pe

prettyColor :: PColor -> Doc -> Doc
prettyColor (PColorCard c) = case c of
  BaseResource -> id
  AdvancedResource -> id
  Infrastructure -> PP.annotate (colorDull Cyan)
  ResearchDevelopment -> PP.annotate (color Green)
  Commercial -> PP.annotate (colorDull Yellow)
  HeadHunting -> PP.annotate (color Red)
  Community -> PP.annotate (color Magenta)
prettyColor (PColorVictory v) = case v of
  PoachingVictory -> PP.annotate (color Red)
  FundingVictory -> PP.annotate (colorDull Yellow)
  CompanyVictory -> id
  InfrastructureVictory -> PP.annotate (colorDull Blue)
  RnDVictory -> PP.annotate (colorDull Green)
  CommercialVictory -> PP.annotate (colorDull Red)
  CommunityVictory -> PP.annotate (color Magenta)

prettyAnsi :: PrettyDoc -> PP.Doc AnsiStyle
prettyAnsi e = case e of
  PEmpty -> PP.emptyDoc
  PCat a b -> mappend (prettyAnsi a) (prettyAnsi b)
  RawText t -> PP.pretty t
  NewLine -> PP.line
  Space -> PP.space
  Emph d -> PP.annotate bold (prettyAnsi d)
  Colorize c d -> prettyColor c (prettyAnsi d)
  Indent n d -> PP.indent n (prettyAnsi d)
  PDirection Own -> "⇓"
  PDirection (Neighboring n) -> prettyAnsi (PNeighbor n)
  PNeighbor NLeft -> "◀ "
  PNeighbor NRight -> "▶ "
  PFund (Funding 0) -> mempty
  PFund (Funding n) -> PP.annotate (color Yellow) (mappend (prettyAnsi (numerical n)) "$")
  PPoach (Poacher 0) -> mempty
  PPoach (Poacher p) ->
    PP.annotate (color Red) $
      if p > 5
        then fromString (replicate (fromIntegral p) '⚔')
        else mappend (prettyAnsi (numerical p)) "⚔"
  PVictory vp -> prettyAnsi $ numerical vp
  PPlayerCount pc -> prettyAnsi $ numerical pc
  PTurn t -> prettyAnsi $ numerical t
  PResource Hype -> PP.annotate (color Cyan) "H"
  PResource Adoption -> PP.annotate (colorDull White) "A"
  PResource Vision -> PP.annotate (color Magenta) "V"
  PResource Development -> PP.annotate (colorDull Yellow) "D"
  PResource Marketing -> PP.annotate (colorDull Green) "M"
  PResource Finance -> PP.annotate (colorDull White) "F"
  PResource Operations -> PP.annotate (color White) "O"
  PAge Age1 -> "Ⅰ"
  PAge Age2 -> "Ⅱ"
  PAge Age3 -> "Ⅲ"
  PCompanyStage Project -> "."
  PCompanyStage Stage1 -> "_"
  PCompanyStage Stage2 -> "="
  PCompanyStage Stage3 -> "Δ"
  PCompanyStage Stage4 -> "☥"
  PConflict Defeat -> PP.annotate (color Red) "-1"
  PConflict (Victory Age1) -> PP.annotate (color Red) "+1"
  PConflict (Victory Age2) -> PP.annotate (color Red) "+3"
  PConflict (Victory Age3) -> PP.annotate (color Red) "+5"
  PCardType t -> prettyColor (PColorCard t) "🃏 "
  PResearch CustomSolution -> PP.annotate (colorDull Green) "⚡ "
  PResearch Programming -> PP.annotate (colorDull Green) "λ "
  PResearch Scaling -> PP.annotate (colorDull Green) "⚖ "
  PCompany (CompanyProfile c s) -> mappend (PP.pretty (show c)) (PP.pretty (show s))

$(deriveBoth defaultOptions ''PColor)
$(deriveBoth defaultOptions ''PrettyDoc)
