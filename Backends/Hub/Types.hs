{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backends.Hub.Types where

import Startups.Base
import Startups.Cards
import Startups.GameTypes
import Startups.Interpreter
import Startups.PrettyPrint

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.List.NonEmpty
import Data.Monoid

import Control.Lens
import Control.Concurrent
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Identity
import Control.Monad.Error

import STM.Promise

type GameId    = Integer
type BackendId = T.Text
type PubFPM    = PubFP Message
type SubFPM    = SubFP Message

type family InteractionResult a :: *
type instance InteractionResult IAskingAction     = (PlayerAction, Exchange)
type instance InteractionResult IAskingCard       = Card
type instance InteractionResult CommunicationType = ()

data IAskingAction   = IAskingAction PlayerId Age (NonEmpty Card) GameState Turn
data IAskingCard     = IAskingCard   PlayerId Age (NonEmpty Card) GameState Message
data ISimpleMessage  = ISimpleMessage CommunicationType GameId GameState

data Interaction = AskingAction   IAskingAction     (PubFPM (InteractionResult IAskingAction))
                 | AskingCard     IAskingCard       (PubFPM (InteractionResult IAskingCard))
                 | SendingMessage ISimpleMessage    (PubFPM (InteractionResult ISimpleMessage))

-- | Most of the actions we are going to run can fail one way or another.
type CanFail = ErrorT Message IO

-- | A backend represents a set of functions used to communicate with
-- a given player. On a first approximation, there will be no support for
-- the "public channel".
--
-- All of these functions must quickly return !
data Backend = Backend { _backendDescription    :: PrettyDoc -- ^ A user friendly description of this backend
                       , _backendId             :: BackendId -- ^ The backend must guarantee the uniqueness of this value
                       , _backendChan           :: TChan Interaction
                       , _backendTerminate      :: IO () -- ^ Can be used to properly terminate a resource (sending a last message for example).
                       }

class Communicable a where
    toInteraction :: a -> PubFPM (InteractionResult a) -> Interaction
    mustReplay    :: a -> Maybe PlayerId

instance Communicable IAskingAction where
    toInteraction = AskingAction
    mustReplay (IAskingAction pid _ _ _ _) = Just pid

instance Communicable IAskingCard where
    toInteraction = AskingCard
    mustReplay (IAskingCard pid _ _ _ _) = Just pid

instance Communicable ISimpleMessage where
    toInteraction = SendingMessage
    mustReplay _ = Nothing

data HGameState = Joining
                | Started ThreadId
                deriving Eq

data BPlayerState = Idle
                  | Communicating Interaction

data PInfos = PInfos { _pgame     :: Maybe GameId
                     -- | The last communication with the player is stored
                     -- here. This is used to support switching backends,
                     -- so that the last question can be re-asked.
                     --
                     -- It is required to set it back to Idle once the
                     -- game has received the answer (ie. getPromise).
                     , _pstate    :: BPlayerState
                     }
-- | Invariants
-- Finished games are not part of the games map
-- All players of a game have an entry in the actor map
-- All players in the actor map participate in zero or one game
data HubState = HubState { _games      :: M.Map GameId HGame
                         , _actors     :: M.Map PlayerId PInfos
                         , _hbackends  :: [Backend]
                         , _comChannel :: TChan Interaction
                         }

data HGame = HGame { _gname    :: T.Text
                   , _gstate   :: HGameState
                   , _gplayers :: [PlayerId]
                   }

type HS = TVar HubState

makeLenses ''Backend
makeLenses ''HubState
makeLenses ''HGame
makeLenses ''PInfos

-- returns true when the pstate is Idle for a player.
isIdle :: PInfos -> Bool
isIdle pinfos = case pinfos ^. pstate of
                    Idle -> True
                    _ -> False

-- | This function will do all the dispatching of player messages. It will
-- return a subscription that will return something when the first backend
-- answers.
communicate :: Communicable a => HS -> a -> CanFail (SubFPM (InteractionResult a))
communicate hs a = join . liftIO . atomically $ communicateSTM hs a

communicateSTM :: Communicable a => TVar HubState -> a -> STM (CanFail (SubFPM (InteractionResult a)))
communicateSTM hs a = do
    gs <- readTVar hs
    (pub, sub) <- newFPromise
    ecom <- case mustReplay a of
        Just pid -> case gs ^? actors . ix pid . pstate of
                        Just Idle -> do
                            let setPState s = modifyTVar hs (actors . ix pid . pstate .~ s)
                                pub' = addPubAction (setPState Idle) pub
                                ncom = toInteraction a pub'
                            setPState (Communicating ncom)
                            return (Right ncom)
                        Just _    -> retry
                        Nothing -> return (Left ("Can't find" <+> showPlayerId pid <+> "to communicate with him!"))
        Nothing -> return (Right (toInteraction a pub))
    case ecom of
        Right com -> do
            mapM_ (`writeTChan` com) (gs ^.. hbackends . traverse . backendChan)
            return (return sub)
        Left rr -> return (throwError rr)

hubDictionnary :: GameId -> HS -> OperationDict SubFPM CanFail
hubDictionnary gameid hs = OperationDict (Strategy pd ac) getpromise message
    where
        getpromise = liftIO . atomically . getResult
        pd age turn pid necards gs = communicate hs (IAskingAction pid age necards gs turn)
        ac age pid necards gs msg = communicate hs (IAskingCard pid age necards gs msg)
        message gs m = void $ communicate hs (ISimpleMessage m gameid gs)

-- | Creates a new game and returns its ID
newGameT :: HS -> STM GameId
newGameT hs = do
    gs <- readTVar hs
    let gamemap = gs ^. games
        n = if M.null gamemap
                then 0
                else fst (M.findMax gamemap) + 1
    writeTVar hs (gs & games . at n ?~ HGame mempty Joining [])
    return n

-- | Add a new player to a game. Warning, in case of error it does nothing
-- at all ! Yes this sucks, I will have to do it another way.
addPlayerT :: HS -> PlayerId -> GameId -> STM (Either PrettyDoc ())
addPlayerT hs pid gameid = do
    gs <- readTVar hs
    let checks = do
            unless (has (games . ix gameid) gs) $
                throwError ("Could not add player to nonexistent game" <+> numerical gameid)
            unless (has (actors . ix pid) gs) $
                throwError (showPlayerId pid <+> "is not registered with the server")
            unless (has (actors . ix pid . pgame . _Nothing) gs) $
                throwError (showPlayerId pid <+> "is already participating in a game")
            unless (gs ^? games . ix gameid . gstate == Just Joining) $
                throwError ("Game" <+> numerical gameid <+> "is not currently accepting players")
    case checks of
        Left _ -> return checks
        Right () -> Right <$> modifyTVar hs
                        ( (games . ix gameid . gplayers %~ (pid :))
                        . (actors . ix pid . pgame ?~ gameid)
                        )

-- | Starts a game given a threadid ... or fails.
startGameT :: HS -> GameId -> ThreadId -> STM (Either PrettyDoc ())
startGameT hs gameid tid = do
    gs <- readTVar hs
    if gs ^? games . ix gameid . gstate == Just Joining
        then Right <$> modifyTVar hs ( games . ix gameid . gstate .~ Started tid )
        else return $ Left  ("Game" <+> numerical gameid <+> "has already started")



