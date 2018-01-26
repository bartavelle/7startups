{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module Startups.GameTypes where

import Startups.Json
import Startups.Base
import Startups.Cards
import Startups.PrettyPrint

import Control.Lens hiding ((.=))

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad.Operational
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.List.NonEmpty
import System.Random
import Data.Aeson
import Elm.Derive

type PlayerId = T.Text

showPlayerId :: PlayerId -> PrettyDoc
showPlayerId = emph . pe

data GameState = GameState { _playermap   :: M.Map PlayerId PlayerState
                           , _discardpile :: [Card]
                           , _rnd         :: !StdGen
                           } deriving Show

type Neighborhood = (PlayerId, PlayerId)

data PlayerState = PlayerState { _pCompany         :: !CompanyProfile
                               , _pCompanyStage    :: !CompanyStage
                               , _pCards           :: [Card]
                               , _pFunds           :: !Funding
                               , _pNeighborhood    :: !Neighborhood
                               , _pPoachingResults :: [PoachingOutcome]
                               } deriving Show

makeLenses ''GameState
makeLenses ''PlayerState

cardEffects :: Traversal' PlayerState Effect
cardEffects = pCards . traverse . cEffect . traverse
{-# INLINE cardEffects #-}

playerEffects :: PlayerId -> Traversal' GameState Effect
playerEffects pid = playermap . ix pid . cardEffects
{-# INLINE playerEffects #-}

neighbor :: Neighbor -> Lens' PlayerState PlayerId
neighbor NLeft  = pNeighborhood . _1
neighbor NRight = pNeighborhood . _2

type Message = PrettyDoc

data PlayerAction = PlayerAction ActionType Card
                  deriving (Show, Eq)
data ActionType = Play | Drop | BuildCompany
                deriving (Show, Eq, Enum, Bounded)

-- | A data structure that represents special capabilities that are used
-- -- when playing a card.
data SpecialInformation = UseOpportunity
                        deriving (Show, Eq)

_NonEmpty :: Prism' [a] (NonEmpty a)
_NonEmpty = prism' toList nonEmpty

-- | This describe the capabilities needed to write the rules, when no
-- interaction with the player is required.
type NonInteractive m = (MonadState GameState m, MonadError Message m, Functor m, Applicative m)
type GameStateOnly m = (MonadState GameState m, Functor m, Applicative m)

data CommunicationType = PlayerCom PlayerId Communication
                       | BroadcastCom Communication
                       deriving Show

data ActionRecap
  = ActionRecap
  { _arAge     :: !Age
  , _arTurn    :: !Turn
  , _arPlayers :: M.Map PlayerId PlayerState
  , _arActions :: M.Map PlayerId (PlayerAction, Exchange, Maybe SpecialInformation)
  } deriving Show

data Communication = RawMessage PrettyDoc
                   | ActionRecapMsg ActionRecap
                   deriving Show

data GameInstr p a where
    PlayerDecision :: Age -> Turn -> PlayerId -> NonEmpty Card -> GameInstr p (p (PlayerAction, Exchange, Maybe SpecialInformation))
    AskCard        :: Age -> PlayerId -> NonEmpty Card -> Message -> GameInstr p (p Card)
    GetPromiseCard :: p Card -> GameInstr p Card
    GetPromiseAct  :: p (PlayerAction, Exchange, Maybe SpecialInformation) -> GameInstr p (PlayerAction, Exchange, Maybe SpecialInformation)
    Message        :: CommunicationType -> GameInstr p ()
    ThrowError     :: Message -> GameInstr p a -- ^ Used for the error instance
    CatchError     :: GameMonad p a -> (Message -> GameMonad p a) -> GameInstr p a

type GameMonad p = ProgramT (GameInstr p) (State GameState)

instance MonadError PrettyDoc (ProgramT (GameInstr p) (State GameState)) where
    throwError = singleton . ThrowError
    catchError a handler = singleton (CatchError a handler)

class (Monad m, MonadError PrettyDoc m, MonadState GameState m) => GMonad p m | m -> p where
    -- | Ask the player which card he would like to play.
    playerDecision   :: Age -> Turn -> PlayerId -> NonEmpty Card -> m (p (PlayerAction, Exchange, Maybe SpecialInformation))
    askCard          :: Age -> PlayerId -> NonEmpty Card -> Message -> m (p Card)
    -- | Tell some information to a specific player
    tellPlayer       :: PlayerId -> Message -> m ()
    -- | Broadcast some information
    generalMessage   :: Message -> m ()
    -- | Awaits a "card" promise
    getPromiseCard   :: p Card -> m Card
    -- | Awaits an "action" promise
    getPromiseAction :: p (PlayerAction, Exchange, Maybe SpecialInformation) -> m (PlayerAction, Exchange, Maybe SpecialInformation)
    -- | Gives a quick rundown of all actions
    actionRecap      :: Age -> Turn -> M.Map PlayerId (PlayerAction, Exchange, Maybe SpecialInformation) -> m ()

instance GMonad p (GameMonad p) where
    playerDecision a t p c = singleton (PlayerDecision a t p c)
    tellPlayer p = singleton . Message . PlayerCom p . RawMessage
    generalMessage = singleton . Message . BroadcastCom . RawMessage
    getPromiseCard = singleton . GetPromiseCard
    getPromiseAction = singleton . GetPromiseAct
    actionRecap age turn mm = get >>= \s -> singleton . Message . BroadcastCom $ ActionRecapMsg (ActionRecap age turn (s ^. playermap) mm)
    askCard a p cl m = singleton (AskCard a p cl m)

-- | Ask the player to chose a card, along with a descriptive message.
-- This is used for the Recycling and CopyCommunity effects.
-- We define a "safe" version of the `askCard` function, that makes sure the
-- player doesn't introduce a new card in the game.
askCardSafe :: GMonad p m => Age -> PlayerId -> NonEmpty Card -> Message -> m Card
askCardSafe a p cl m = do
    card <- askCard a p cl m >>= getPromiseCard
    when (card `notElem` (cl ^. re _NonEmpty)) (throwError (showPlayerId p <+> "tried to play a non proposed card"))
    return card

instance PrettyE PlayerAction where
    pe (PlayerAction a c) = a' <+> cardName c
        where
            a' = case a of
                     Play         -> "played"
                     Drop         -> "dropped"
                     BuildCompany -> "increase the company stage"

$(deriveBoth baseOptions ''PlayerState)
$(deriveBoth baseOptions ''PlayerAction)
$(deriveBoth baseOptions ''ActionType)
$(deriveBoth baseOptions ''ActionRecap)
$(deriveElmDef baseOptions ''SpecialInformation)

instance ToJSON SpecialInformation where
  toJSON UseOpportunity = String "UseOpportunity"

instance FromJSON SpecialInformation where
  parseJSON = withText "SpecialInformation" $ \t -> UseOpportunity <$ guard (t == "UseOpportunity")

instance ToJSON GameState where
    toJSON (GameState pm dp _) = object [ "playermap" .= pm
                                        , "discardpile" .= dp
                                        ]


