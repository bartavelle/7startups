{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Backends.Hub where

import Startups.Base
import Startups.Cards
import Startups.GameTypes
import Startups.Utils
import Startups.Game
import Startups.Interpreter
import Startups.PrettyPrint
import Backends.Common
import STM.Promise

import qualified Data.Map.Strict as M
import qualified Data.Foldable as F
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid

import Control.Lens
import Control.Applicative
import Control.Concurrent (ThreadId,killThread)
import Control.Concurrent.STM
import Control.Monad.State.Strict (State,gets)
import System.Random (newStdGen)

import MVC

type GameId = Integer
type PubFPM    = PubFP Message
type SubFPM    = SubFP Message

data PlayerInput = NumericChoice Int
                 | Start   GameId
                 | Stop    GameId
                 | Join    (Maybe GameId)
                 | Go
                 | NotGo
                 | Leave
                 | MyStartup
                 | ShortSituation
                 | DetailedSituation
                 | CustomCommand T.Text -- ^ This sucks a bit, but is used for custom interbackend communication
                 deriving (Eq, Show)

data IAskingAction   = IAskingAction PlayerId Age (NonEmpty Card) GameState Turn
data IAskingCard     = IAskingCard   PlayerId Age (NonEmpty Card) GameState Message

data IAsk = AskingAction   IAskingAction  (PubFPM (PlayerAction, Exchange))
          | AskingCard     IAskingCard    (PubFPM Card)

getPid :: IAsk -> PlayerId
getPid (AskingAction (IAskingAction pid _ _ _ _) _) = pid
getPid (AskingCard   (IAskingCard   pid _ _ _ _) _) = pid

getGameState :: IAsk -> GameState
getGameState (AskingAction (IAskingAction _ _ _ gs _) _) = gs
getGameState (AskingCard   (IAskingCard   _ _ _ gs _) _) = gs

ask2message :: IAsk -> Message
ask2message (AskingAction (IAskingAction pid age necards gs _) _) =
    let ps = gs ^. playermap
        choices = allowableActions age pid necards ps
    in  playerActionsDialog pid ps necards choices
ask2message (AskingCard (IAskingCard pid _ necards gs msg) _) =
    let ps = gs ^. playermap
    in  msg </> cardChoiceDialog pid ps (F.toList necards)

data Interaction = Asking IAsk
                 | SendingMessage CommunicationType
                 | GameOver (M.Map PlayerId (M.Map VictoryType VictoryPoint))

hubDictionnary :: (Interaction -> STM ()) -> OperationDict SubFPM IO
hubDictionnary hubinput = OperationDict (Strategy pd ac) getpromise message
    where
        getpromise                 = atomically . getResult
        pd age turn pid necards gs = writeInput $ AskingAction (IAskingAction pid age necards gs turn)
        ac age pid necards gs msg  = writeInput $ AskingCard   (IAskingCard pid age necards gs msg)
        message _ m                = atomically (hubinput (SendingMessage m))
        writeInput :: (PubFPM a -> IAsk) -> IO (SubFPM a)
        writeInput i = atomically $ do
            (p, s) <- newPromise
            hubinput (Asking (i p))
            return s

data ControllerInput = GInput GameId Interaction
                     | PInput PlayerId PlayerInput

data VOutput = SpawnGame GameId [PlayerId]
             | KillGame GameId
             | DropGame GameId
             | TellPlayer PlayerId Message
             | Broadcast Message
             | FailAsk Message IAsk -- used when the player is not in the proper state
             | SucceedAskingAction (PlayerAction, Exchange) (PubFPM (PlayerAction, Exchange))
             | SucceedAskingCard   Card                     (PubFPM Card)
             | OCustom T.Text -- ^ Custom backend communication

type HubState = M.Map GameId GameS

data GameS = GameJoining (M.Map PlayerId PlayerJoining)
           | GamePlaying (M.Map PlayerId PlayerPlaying)

data PlayerJoining = Joined
                   | Ready

data PlayerPlaying = InGame (Maybe GameState) -- stores the last gamestate
                   | Awaiting IAsk

makePrisms ''GameS
makePrisms ''PlayerJoining
makePrisms ''PlayerPlaying
makePrisms ''VOutput

type ModelM = ListT (State HubState)

playerDesc :: GameId -> ModelM Message
playerDesc = fmap (maybe mempty (foldPretty . desc)) . preuse . ix
    where
        desc (GameJoining mj) = itoList mj & map (uncurry pj)
        desc (GamePlaying mp) = itoList mp & map (uncurry pl)
        pj pid Joined = showPlayerId pid
        pj pid Ready = "!" <> showPlayerId pid
        pl pid (InGame _) = showPlayerId pid
        pl pid (Awaiting _) = "⌛" <> showPlayerId pid

filterGames :: (GameS -> Bool) -> ModelM [(GameId, GameS)]
filterGames prd = gets (itoListOf (itraversed . filtered prd))

handleJoin :: PlayerId -> Maybe GameId -> ModelM VOutput
-- There are 3 cases when the player did not submit a game id.
--
-- Case 1 : no game are accepting players, we must create one first
-- Case 2 : a single game is accepting players.
-- Case 3 : several games are accepting players.
handleJoin pid i =
    let joinMessage gameid = (\pd -> Broadcast (showPlayerId pid <+> "joined game" <+> numerical gameid <+> pd)) <$> playerDesc gameid
    in case i of
           Nothing -> do
               -- get the list of accepting games that are not full (7 players)
               acceptingGames <- map fst <$> filterGames (has (_GameJoining . to M.size . filtered (<7)))
               case acceptingGames of
                   [] -> do
                       let gm x | M.null x = 0
                                | otherwise = succ (fst (M.findMax x))
                       gameid <- gets gm
                       at gameid ?= GameJoining (M.singleton pid Joined)
                       return (Broadcast ("New game" <+> numerical gameid)) <> joinMessage gameid
                   [gameid] -> do
                       ix gameid . _GameJoining . at pid ?= Joined
                       joinMessage gameid
                   gameids -> return (TellPlayer pid ("Too many games are currently accepting players, please choose between:" <+> foldPretty (map numerical gameids)))
           Just gameid -> do
               stt <- use (at gameid)
               case stt of
                   Nothing -> return (TellPlayer pid "This game doesn't exist")
                   Just g -> case g ^? _GameJoining of
                                 Nothing -> return (TellPlayer pid "This game has already started")
                                 Just pl -> if M.size pl >= 7
                                                then return (TellPlayer pid "This game is full")
                                                else do
                                                    ix gameid . _GameJoining . at pid ?= Joined
                                                    joinMessage gameid

startGame :: GameId -> ModelM VOutput
startGame gameid = preuse (ix gameid) >>= \gameinfo -> case gameinfo of
    Nothing -> return (Broadcast ("Game" <+> numerical gameid <+> "was not found"))
    Just (GamePlaying _) -> return (Broadcast ("Game" <+> numerical gameid <+> "is already started."))
    Just (GameJoining m) ->
        if M.size m >= 3 && M.size m <= 7
            then do
                at gameid ?= GamePlaying (m & traverse .~ InGame Nothing)
                return (SpawnGame gameid (itoList m & map fst))
            else return (Broadcast "A 7 startups game must have between 3 and 7 players")

-- | This is the "model" of our MVC part, getting ControllerInput and
-- returning VOutput.
hub :: Model HubState ControllerInput VOutput
hub = asPipe (loop h)
    where
        -- gets the gameid a player in a specific state is
        gameIdWithPrism :: PlayerId -> Traversal' PlayerJoining () -> ModelM (Maybe GameId)
        gameIdWithPrism pid pjprism = do
            playerGame <- filterGames (has (_GameJoining . ix pid . pjprism))
            case playerGame of
                [(gameid,_)] -> return (Just gameid)
                _ -> return Nothing
        h :: ControllerInput -> ModelM VOutput
        h i = case i of
                  GInput gameid gmi -> handleGameInput gameid gmi
                  PInput pid pli -> handlePlayerInput pid pli
        getLastState pid = do
            ma <- gets $ preview (traverse . _GamePlaying . ix pid)
            return $ case ma of
                Just (InGame (Just gs)) -> Just gs
                Just (Awaiting a) -> Just (getGameState a)
                _ -> Nothing
        bc = return . Broadcast
        tp pid = return . TellPlayer pid
        handlePlayerInput :: PlayerId -> PlayerInput -> ModelM VOutput
        handlePlayerInput pid i = case i of
            CustomCommand x -> return (OCustom x)
            Join x -> handleJoin pid x
            NumericChoice n -> do
                let notifySuccess a gs choicelist successFunction =
                        case choicelist ^? ix n of
                            Just c -> do
                                traverse . _GamePlaying . ix pid .= InGame (Just gs)
                                return (successFunction c) <> bc (showPlayerId pid <+> "played")
                            _ -> tp pid (ask2message a)
                ma <- gets $ preview (traverse . _GamePlaying . ix pid . _Awaiting)
                case ma of
                    Nothing -> mempty
                    Just a@(AskingCard   (IAskingCard _ _ necards gs _) pub) ->
                        notifySuccess a gs necards (`SucceedAskingCard` pub)
                    Just a@(AskingAction (IAskingAction _ age necards gs _) pub) -> do
                        let allowable = allowableActions age pid necards (gs ^. playermap)
                                            & traverse %~ (\(x,y,_) -> (x,y))
                        notifySuccess a gs allowable (`SucceedAskingAction` pub)
            -- Go is only acceptable when the player is in Joined state
            Go -> let hgid gameid = do
                        ix gameid . _GameJoining . at pid ?= Ready
                        gm <- use (ix gameid . _GameJoining . below _Ready)
                        pdesc <- playerDesc gameid
                        bc ("Player" <+> showPlayerId pid <+> "is ready" <+> pdesc)
                            <> if M.size gm >= 3 -- if all players are ready, start game
                                   then handlePlayerInput pid (Start gameid)
                                   else mempty
                  in  gameIdWithPrism pid _Joined >>= maybe mempty hgid
            NotGo -> let hgid gameid = do
                            ix gameid . _GameJoining . at pid ?= Joined
                            pdesc <- playerDesc gameid
                            bc ("Player" <+> showPlayerId pid <+> "is not ready" <+> pdesc)
                     in  gameIdWithPrism pid _Ready >>= maybe mempty hgid
            Start gameid -> startGame gameid
            Stop gameid -> do
                at gameid .= Nothing
                return (KillGame gameid)
            Leave -> let hgid gameid = do
                            ix gameid . _GameJoining . at pid .= Nothing
                            pdesc <- playerDesc gameid
                            bc ("Player" <+> showPlayerId pid <+> "left the game" <+> pdesc)
                     in  gameIdWithPrism pid (const pure) >>= maybe mempty hgid
            MyStartup -> getLastState pid >>= tp pid . maybe "Can't find the last game state" pst
                                                     . preview (traverse . playermap . ix pid)
                where pst ps = playerStartup (ps ^. pCompany) (ps ^. pCompanyStage)
            ShortSituation -> getLastState pid >>= tp pid . maybe "Can't find the last game state" (situationRecap . _playermap)
            DetailedSituation -> getLastState pid >>= tp pid . maybe "Can't find the last game state" (detailedSituationRecap . _playermap)
        handleGameInput :: GameId -> Interaction -> ModelM VOutput
        handleGameInput gameid i = case i of
            SendingMessage (PlayerCom pid c) -> tp pid (displayCommunication c)
            SendingMessage (BroadcastCom c) -> bc (displayCommunication c)
            GameOver score -> do
                at gameid .= Nothing
                return (Broadcast ("Game" <+> numerical gameid <+> "is over" </> displayVictory score))
                    <> return (DropGame gameid)
            Asking a -> do
                let pid = getPid a
                mstate <- gets $ preview (ix gameid . _GamePlaying . ix pid)
                case mstate of
                    Just (InGame _) -> do
                        ix gameid . _GamePlaying . ix pid .= Awaiting a
                        return (TellPlayer pid (ask2message a))
                    Just _ -> return (FailAsk "Can't ask a player that is not waiting for an answer" a)
                    Nothing -> return (FailAsk "Player is not in the correct state" a)

data Backend = Backend { _backendInput     :: STM (PlayerId, PlayerInput)
                       , _backendTell      :: PlayerId -> Message -> IO ()
                       , _backendBroadcast :: Message -> IO ()
                       , _backendCustom    :: T.Text -> IO ()
                       }

runSucceed :: a -> PubFPM a -> IO ()
runSucceed i prom = atomically (fulfillPromise prom i)

runFail :: Message -> IAsk -> IO ()
runFail msg ia = atomically $ case ia of
    AskingAction _ prom -> failPromise prom msg
    AskingCard   _ prom -> failPromise prom msg

handleOutput :: (GameId -> Interaction -> STM ()) -- ^ Will be used by the games to talk back to the controllers
             -> (PlayerId -> Message -> IO ()) -- ^ A summary of all backends tells
             -> (Message -> IO ()) -- ^ A summary of all backends broadcasts
             -> (T.Text -> IO ()) -- ^ A summary of all backends custom handlers
             -> TVar (M.Map GameId ThreadId) -- ^ The thread list
             -> View VOutput
handleOutput gameoutput betell bebc becu tthreads =
    let dropgame gameid = atomically $ modifyTVar tthreads (at gameid .~ Nothing)
        killgame gameid = do
            readTVarIO tthreads >>= F.mapM_ killThread
            dropgame gameid
        spawnGame gameid playerlist = do
            g <- newStdGen
            let dico = hubDictionnary (gameoutput gameid)
                initstate = initialGameState g playerlist
            tid <- forkIO $ do
                (_, o) <- runInterpreter dico initstate playGame
                atomically $ gameoutput gameid $ case o of
                    Left rr       -> SendingMessage $ BroadcastCom $ RawMessage rr
                    Right results -> GameOver results
            atomically $ modifyTVar tthreads (at gameid ?~ tid)

    in asSink $ \i -> case i of
        -- custom backend messages
        OCustom x                   -> becu x
        -- game managing messages
        DropGame gameid             -> dropgame gameid
        KillGame gameid             -> killgame gameid
        SpawnGame gameid playerlist -> spawnGame gameid playerlist
        -- game promise fulfilment
        SucceedAskingAction a prom  -> runSucceed a prom
        SucceedAskingCard a prom    -> runSucceed a prom
        FailAsk msg ia              -> runFail msg ia
        -- messaging
        TellPlayer pid msg          -> betell pid msg
        Broadcast msg               -> bebc msg


runHub :: [Backend] -> IO ()
runHub backends = do
    gamequeue <- newTQueueIO
    gamethreads <- newTVarIO mempty :: IO (TVar (M.Map GameId ThreadId))
    let gameinput = asInput (Input (Just . uncurry GInput <$> readTQueue gamequeue))
        backendsinput = F.foldMap (asInput . Input . fmap (Just . uncurry PInput) . _backendInput) backends
        gameoutput = curry (writeTQueue gamequeue)
        betell pid msg = mapM_ (\b -> _backendTell b pid msg) backends
        bebc msg = mapM_ (`_backendBroadcast` msg) backends
        becu msg = mapM_ (`_backendCustom` msg) backends
        external :: Managed (View VOutput, Controller ControllerInput)
        external = return (handleOutput gameoutput betell bebc becu gamethreads, gameinput <> backendsinput)
    void $ runMVC mempty hub external
