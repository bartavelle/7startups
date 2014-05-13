{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Startups.GameTypes where

import Startups.Base
import Startups.Cards
import Startups.PrettyPrint

import Control.Lens

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad.Operational
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

data GameInstr a where
    PlayerDecision :: Age -> Turn -> PlayerId -> [Card] -> GameInstr (PlayerAction, Exchange)
    AskCard        :: Age -> PlayerId -> NonEmpty Card -> Message -> GameInstr Card
    TellPlayer     :: PlayerId -> Message -> GameInstr ()
    GeneralMessage :: Message -> GameInstr ()
    ThrowError     :: Message -> GameInstr a -- ^ Used for the error instance
    CatchError     :: GameMonad a -> (Message -> GameMonad a) -> GameInstr a

type GameMonad = ProgramT GameInstr (State GameState)

-- | Ask the player which card he would like to play.
playerDecision :: Age -> Turn -> PlayerId -> [Card] -> GameMonad (PlayerAction, Exchange)
playerDecision a t p c = singleton (PlayerDecision a t p c)

-- | Tell some information to a specific player
tellPlayer :: PlayerId -> Message -> GameMonad ()
tellPlayer p = singleton . TellPlayer p

-- | Broadcast some information
generalMessage :: Message -> GameMonad ()
generalMessage = singleton . GeneralMessage

instance MonadError PrettyDoc (ProgramT GameInstr (State GameState)) where
    throwError = singleton . ThrowError
    catchError a handler = singleton (CatchError a handler)

-- | Ask the player to chose a card, along with a descriptive message.
-- This is used for the Recycling and CopyCommunity effects.
-- We define a "safe" version of the `askCard` function, that makes sure the
-- player doesn't introduce a new card in the game.
askCardSafe :: Age -> PlayerId -> NonEmpty Card -> Message -> GameMonad Card
askCardSafe a p cl m = do
    card <- singleton (AskCard a p cl m)
    when (card `notElem` (cl ^. re _NonEmpty)) (throwError (showPlayerId p <+> "tried to play a non proposed card"))
    return card

instance PrettyE PlayerAction where
    pe (PlayerAction a c) = a' <+> cardName c
        where
            a' = case a of
                     Play         -> "played"
                     Drop         -> "dropped"
                     BuildCompany -> "increase the company stage"
