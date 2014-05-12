{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Startups.GameTypes where

import Startups.Base
import Startups.Cards

import Control.Lens

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Monad.Error
import Control.Applicative
import System.Random

type PlayerId = T.Text

data GameState = GameState { _playermap   :: M.Map PlayerId PlayerState
                           , _discardpile :: [Card]
                           , _rnd         :: StdGen
                           }

type Neighborhood = (PlayerId, PlayerId)

data PlayerState = PlayerState { _pCompany         :: CompanyProfile
                               , _pCompanyStage    :: CompanyStage
                               , _pCards           :: [Card]
                               , _pFunds           :: Funding
                               , _pNeighborhood    :: Neighborhood
                               , _pPoachingResults :: [PoachingOutcome]
                               }

makeLenses ''GameState
makeLenses ''PlayerState

cardEffects :: Traversal' PlayerState Effect
cardEffects = pCards . traverse . cEffect . traverse

playerEffects :: PlayerId -> Traversal' GameState Effect
playerEffects pid = playermap . ix pid . cardEffects

neighbor :: Neighbor -> Lens' PlayerState PlayerId
neighbor NLeft  = pNeighborhood . _1
neighbor NRight = pNeighborhood . _2

type Message = String

data PlayerAction = PlayerAction ActionType Card
data ActionType = Play | Drop | BuildCompany

-- | This describe the capabilities needed to write the rules, when no
-- interaction with the player is required.
type NonInteractive m = (MonadState GameState m, Monad m, MonadError Message m, Functor m, Applicative m)
type GameStateOnly m = (MonadState GameState m, Monad m, Functor m, Applicative m)

class NonInteractive m => GameMonad m where
    -- | Ask the player which card he would like to play.
    playerDecision    :: Age -> Turn -> PlayerId -> [Card] -> GameState -> m (PlayerAction, Exchange)
    -- | Ask the player to chose a card, along with a descriptive message.
    -- This is used for the Recycling and CopyCommunity effects.
    askCard           :: Age -> PlayerId -> [Card] -> GameState -> Message -> m Card
    tellPlayer        :: PlayerId -> Message -> m () -- ^ Tell some information to a specific player
    generalMessage    :: Message -> m () -- ^ Broadcast some information

-- We define "safe" versions of the `askCard` function, that makes sure the
-- player doesn't introduce a new card in the game.

askCardSafe :: GameMonad m => Age -> PlayerId -> [Card] -> GameState -> Message -> m Card
askCardSafe a p cl s m = do
    card <- askCard a p cl s m
    when (card `notElem` cl) (throwError "The player tried to play a non proposed card")
    return card
