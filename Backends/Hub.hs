{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backends.Hub where

import Startups.Base
import Startups.Cards
import Startups.GameTypes
import Startups.Game
import Startups.Utils
import Startups.Interpreter
import Startups.PrettyPrint
import Backends.Common
import STM.PubSub

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.List.NonEmpty hiding (length)
import Data.Monoid

import Control.Lens
import Control.Concurrent
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Identity
import Control.Monad.Error

import System.Random

type GameId    = Integer
type BackendId = T.Text
type PubFPM    = PubFP Message
type SubFPM    = SubFP Message

type family InteractionResult a :: *
type instance InteractionResult IAskingAction     = (PlayerAction, Exchange)
type instance InteractionResult IAskingCard       = Card
type instance InteractionResult CommunicationType = ()
type instance InteractionResult ISimpleMessage    = ()

data IAskingAction   = IAskingAction PlayerId Age (NonEmpty Card) GameState Turn
data IAskingCard     = IAskingCard   PlayerId Age (NonEmpty Card) GameState Message
data ISimpleMessage  = ISimpleMessage CommunicationType GameId

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
                       -- ^ Can be used to properly terminate a resource (sending a last message for example).
                       , _backendTerminateGame  :: [PlayerId] -> GameId -> IO ()
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

data GoOrNot = Go | NotGo
             deriving Eq

data HGameState = Joining (M.Map PlayerId GoOrNot) -- ^ Is the player ok with the current game size ?
                | Started (Maybe ThreadId) -- ^ This should get a threadid ASAP
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
                         }

data HGame = HGame { _gname    :: T.Text
                   , _gstate   :: HGameState
                   }

type HS = TVar HubState

makeLenses ''Backend
makeLenses ''HubState
makeLenses ''HGame
makeLenses ''PInfos
makePrisms ''BPlayerState
makePrisms ''Interaction
makePrisms ''HGameState

-- Returns a game the players is part of
getPlayerGame :: HubState -> PlayerId -> Maybe GameId
getPlayerGame gs pid = gs ^? actors . ix pid . pgame . traverse

-- returns true when the pstate is Idle for a player.
isIdle :: PInfos -> Bool
isIdle pinfos = case pinfos ^. pstate of
                    Idle -> True
                    _ -> False

newHubState :: IO HS
newHubState = newTVarIO (HubState mempty mempty [])

-- | This function will do all the dispatching of player messages. It will
-- return a subscription that will return something when the first backend
-- answers.
communicate :: Communicable a => HS -> a -> CanFail (SubFPM (InteractionResult a))
communicate hs a = join . liftIO . atomically $ communicateSTM hs a

broadcast :: HS -> GameId -> Message -> STM ()
broadcast hs gameid msg = void $ communicateSTM hs (ISimpleMessage (BroadcastCom (RawMessage msg)) gameid)

communicateSTM :: Communicable a => TVar HubState -> a -> STM (CanFail (SubFPM (InteractionResult a)))
communicateSTM hs a = do
    gs <- readTVar hs
    (pub, sub) <- newPubSub
    ecom <- case mustReplay a of
        Just pid -> case gs ^? actors . ix pid . pstate of
                        Just Idle -> do
                            let setPState s = modifyTVar' hs (actors . ix pid . pstate .~ s)
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
        message _ m = void $ communicate hs (ISimpleMessage m gameid)

-- | Creates a new game and returns its ID
newGameT :: HS -> STM GameId
newGameT hs = do
    gs <- readTVar hs
    let gamemap = gs ^. games
        n = if M.null gamemap
                then 0
                else fst (M.findMax gamemap) + 1
    writeTVar hs (gs & games . at n ?~ HGame mempty (Joining mempty))
    broadcast hs n ("New game" <+> numerical n)
    return n

-- | Add a new player to a game. Warning, in case of error it does nothing
-- at all ! Yes this sucks, I will have to do it another way.
addPlayerT :: HS -> PlayerId -> GameId -> STM (Either PrettyDoc ())
addPlayerT hs pid gameid = do
    gs <- readTVar hs
    let joinMap = gs ^? games . ix gameid . gstate . _Joining
        checks = do
            unless (has (games . ix gameid) gs) $
                throwError ("Could not add player to nonexistent game" <+> numerical gameid)
            unless (has (actors . ix pid) gs) $
                throwError (showPlayerId pid <+> "is not registered with the server")
            unless (has (actors . ix pid . pgame . _Nothing) gs) $
                throwError (showPlayerId pid <+> "is already participating in a game")
            case joinMap of
                Nothing -> throwError ("Game" <+> numerical gameid <+> "is not currently accepting players")
                Just jm -> when (M.size jm >= 7) $
                    throwError ("Game" <+> numerical gameid <+> "is full")
    case checks of
        Left _ -> return checks
        Right () -> do
            broadcast hs gameid ("New player:" <+> showPlayerId pid <> "," <+> "current player list:" <+> foldPretty (joinMap ^.. traverse . to M.keys . traverse . to showPlayerId))
            Right <$> modifyTVar' hs
                        ( (games . ix gameid . gstate . _Joining . at pid ?~ NotGo)
                        . (actors . ix pid .~ PInfos (Just gameid) Idle)
                        )

-- | Sets a player to "Go". When all players are Go, the game will start.
goPlayer :: HS -> PlayerId -> STM (Either Message (IO ()))
goPlayer hs pid = do
    gs <- readTVar hs
    case getPlayerGame gs pid of
        Just gameid -> do
            let golist = gs ^.. games . ix gameid . gstate . _Joining . traverse
                goodnumber = length golist >= 3 && length golist <= 7
                allgo = all (== Go) golist
            if goodnumber && allgo
                then startGameT hs gameid
                else return (Right (return ()))
        Nothing     -> return (Left "Player is not part of a game")

-- | Not to be run directly, should be "joined" from "startGameT". We
-- expect the game to exist, as it should have been checked by
-- "startGameT". The game can't be stopped too early, as the "stopGame"
-- function should make sure the threadid is set !
runGame :: HS -> GameId -> IO ()
runGame hs gameid = void $ forkIO $ do
    tid <- myThreadId
    pl <- atomically $ do
        playerlist <- M.keys . view (games . ix gameid . gstate . _Joining) <$> readTVar hs
        modifyTVar' hs (games . ix gameid . gstate .~ Started (Just tid))
        return playerlist
    gen <- newStdGen
    em <- runErrorT $ runInterpreter (hubDictionnary gameid hs) (initialGameState gen pl) playGame
    let fem = case em of
                  Left rr -> Left rr
                  Right (_, Left rr) -> Left rr
                  Right (_, Right v) -> Right v
    -- the game is over, let's display the result
    atomically $ broadcast hs gameid $ "Game" <+> numerical gameid <+> case fem of
            Left rr -> "failed with error:" <+> rr
            Right v -> displayVictory v
    -- tell all backends to clean up
    view hbackends <$> readTVarIO hs
        >>= mapM_ (\b -> _backendTerminateGame b pl gameid)
    -- and tear down the game
    atomically $ modifyTVar' hs ( (actors %~ M.filter (hasn't (pgame . _Just . only gameid)))
                                . (games . at gameid .~ Nothing)
                                )

-- | Starts a game given a threadid ... or fails. This won't compose as
-- expected with STM.
startGameT :: HS -> GameId -> STM (Either PrettyDoc (IO ()))
startGameT hs gameid = do
    gs <- readTVar hs
    let checksize plist | M.size plist < 3 = return (Left "Not enough players to start this game")
                        | M.size plist > 7 = return (Left "Too many players ... how did we get there ?")
                        | otherwise = do
                            broadcast hs gameid ("Game" <+> numerical gameid <+> "started!")
                            modifyTVar' hs (games . ix gameid . gstate .~ Started Nothing)
                            return (Right (runGame hs gameid))
    case gs ^? games . ix gameid . gstate . _Joining of
        Just plist -> checksize plist
        _ -> return $ Left  ("Game" <+> numerical gameid <+> "has already started")

-- | Registers a new backend
registerBackend :: HS -> Backend -> STM ()
registerBackend hs backend = modifyTVar' hs (hbackends %~ (backend :))
