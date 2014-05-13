{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Startups.GameTypes where

import Startups.Base
import Startups.Cards
import Startups.PrettyPrint

import Control.Lens

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Control.Monad.Error
import Control.Applicative
import System.Random

type PlayerId = T.Text

showPlayerId :: PlayerId -> PrettyDoc
showPlayerId = emph . pe

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

type Message = PrettyDoc

data PlayerAction = PlayerAction ActionType Card
data ActionType = Play | Drop | BuildCompany

-- | Some types for non empty lists
data NonEmpty a = NonEmpty a [a]

_NonEmpty :: Prism' [a] (NonEmpty a)
_NonEmpty = prism fromNonEmpty toNonEmpty
    where
        fromNonEmpty (NonEmpty x xs) = x : xs
        toNonEmpty l = case l of
                           [] -> Left l
                           (x:xs) -> Right (NonEmpty x xs)

-- | This describe the capabilities needed to write the rules, when no
-- interaction with the player is required.
type NonInteractive m = (MonadState GameState m, Monad m, MonadError Message m, Functor m, Applicative m)
type GameStateOnly m = (MonadState GameState m, Monad m, Functor m, Applicative m)

class NonInteractive m => GameMonad m where
    -- | Ask the player which card he would like to play.
    playerDecision    :: Age -> Turn -> PlayerId -> [Card] -> GameState -> m (PlayerAction, Exchange)
    -- | Ask the player to chose a card, along with a descriptive message.
    -- This is used for the Recycling and CopyCommunity effects.
    askCard           :: Age -> PlayerId -> NonEmpty Card -> GameState -> Message -> m Card
    tellPlayer        :: PlayerId -> Message -> m () -- ^ Tell some information to a specific player
    generalMessage    :: Message -> m () -- ^ Broadcast some information

-- We define a "safe" version of the `askCard` function, that makes sure the
-- player doesn't introduce a new card in the game.
askCardSafe :: GameMonad m => Age -> PlayerId -> NonEmpty Card -> GameState -> Message -> m Card
askCardSafe a p cl s m = do
    card <- askCard a p cl s m
    when (card `notElem` (cl ^. re _NonEmpty)) (throwError (showPlayerId p <+> "tried to play a non proposed card"))
    return card

instance PrettyE PlayerAction where
    pe (PlayerAction a c) = a' <+> cardName c
        where
            a' = case a of
                     Play         -> "played"
                     Drop         -> "dropped"
                     BuildCompany -> "increase the company stage"
