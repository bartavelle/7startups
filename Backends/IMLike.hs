{-# LANGUAGE OverloadedStrings #-}
module Backends.IMLike where

import Backends.Hub
import Backends.Common
import Startups.Base
import Startups.Cards
import Startups.PrettyPrint
import Startups.GameTypes
import Startups.Utils
import STM.PubSub

import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative
import Data.Monoid
import Data.List.NonEmpty (NonEmpty)
import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Map.Strict as M

data Recipient = RPlayer PlayerId
               | RGameChannel GameId
               deriving (Ord, Eq)

data IMState = IMState { _gamestate :: HS
                       , _callbacks :: TVar (M.Map PlayerId Handler)
                       }

type Handler = IMState
            -> PlayerId -- ^ source
            -> [T.Text] -- ^ arguments
            -> TChan (Recipient, Message) -- ^ output channel
            -> STM ( IO() )

retn :: STM (IO ())
retn = return (return ())

toIO :: STM (Either PrettyDoc ()) -> STM (Either PrettyDoc (IO ()))
toIO = fmap (_Right .~ return ())

withError :: IMState -> PlayerId -> TChan (Recipient, Message) -> (HS -> STM (Either PrettyDoc ( IO() ))) -> STM ( IO () )
withError is pid tc f = do
    mr <- f (_gamestate is)
    case mr of
        Right a -> return a
        Left rr  -> writeTChan tc (RPlayer pid, rr)  >> retn


withGame :: T.Text -> (GameId -> STM (Either PrettyDoc (IO ()))) -> STM (Either PrettyDoc ( IO() ))
withGame g m = case T.decimal g of
                 Right (gameid, "") -> m gameid
                 _ -> return (Left "Invalid game id")

handleReady :: Handler
handleReady is pid [tgameid] tc = withError is pid tc $ \hs     -> withGame tgameid
                                                      $ \gameid -> toIO (addPlayerT hs pid gameid)
handleReady _ pid _ tc = writeTChan tc (RPlayer pid, "Error: ready expects a GameId") >> retn


handleGo :: Handler
handleGo is pid _ tc = withError is pid tc $ \hs -> goPlayer hs pid

handleHelp :: Handler
handleHelp _ pid _ tc = writeTChan tc (RPlayer pid, "Available commands:" <+> foldPretty (map (T.cons '!') (M.keys commands))) >> retn

handleStart :: Handler
handleStart is pid [tgameid] tc = withError is pid tc $ \hs     -> withGame tgameid
                                                      $ \gameid -> startGameT hs gameid
handleStart _ pid _ tc = writeTChan tc (RPlayer pid, "Error: start expects a GameId") >> retn

handleStop :: Handler
handleStop = undefined

commands :: M.Map T.Text Handler
commands = M.fromList [ ("ready", handleReady)
                      , ("go"   , handleGo)
                      , ("help" , handleHelp)
                      , ("start", handleStart)
                      , ("stop" , handleStop)
                      ]

terminateGame :: IMState -> [PlayerId] -> GameId -> IO ()
terminateGame is pids _ = atomically (modifyTVar (_callbacks is) (M.filterWithKey (\pid _ -> pid `notElem` pids)))

-- | We can erase a previous callback, which might be bad ...
-- TODO : verify the callback invariant, ie. that we call this function
-- only when there is no callbacks defined
addCallback :: IMState -> PlayerId -> Handler -> STM ()
addCallback is pid handler = modifyTVar (_callbacks is) (at pid ?~ handler)

dropCallback :: IMState -> PlayerId -> STM ()
dropCallback is pid = modifyTVar (_callbacks is) (at pid .~ Nothing)

askChoices :: (PlayerId -> TChan (Recipient, Message) -> STM ())  -> NonEmpty a -> PubFPM a -> Handler
askChoices erra nelist pub is pid responses output = do
    let err = erra pid output
    case responses of
        [response] -> case T.decimal response of
            Right (resp, "") -> case (_NonEmpty # nelist) ^? ix resp of
                                    Just x -> do
                                        fulfillPub pub x
                                        dropCallback is pid
                                    Nothing -> err
            _ -> err
        _ -> err
    retn

-- | Gives numerical choices for the cards, and the default actions.
askCardCallback :: NonEmpty Card -> GameState -> Message -> PubFPM Card -> Handler
askCardCallback necards gs msg =
    askChoices (\pid output -> askCardDialog pid necards gs msg output) necards

askCardDialog :: PlayerId -> NonEmpty Card -> GameState -> Message -> TChan (Recipient, Message) -> STM ()
askCardDialog pid necards gs msg output = writeTChan output (RPlayer pid, msg </> cardChoiceDialog pid (gs ^. playermap) (_NonEmpty # necards))

askActionCallback :: Age -> NonEmpty Card -> GameState -> PubFPM (PlayerAction, Exchange) -> Handler
askActionCallback age necards gs pub is pid =
    let actions = allowableActions age pid necards (gs ^. playermap)
        results = fmap (\(a,b,_) -> (a,b)) actions
    in  askChoices (\plid output -> askActionDialog plid age necards gs output) results pub is pid

askActionDialog :: PlayerId -> Age -> NonEmpty Card -> GameState -> TChan (Recipient, Message) -> STM ()
askActionDialog pid age necards gs output =
    let actions = allowableActions age pid necards pmap
        pmap = gs ^. playermap
    in  writeTChan output (RPlayer pid, playerActionsDialog pid pmap necards actions)

runIMLike :: HS -> PrettyDoc -> BackendId -> TChan (PlayerId, T.Text) -> TChan (Recipient, Message) -> IO Backend
runIMLike hs desc backid input output = do
    is <- IMState <$> pure hs <*> newTVarIO mempty
    interactionChan <- newTChanIO
    -- the part that listens to requests from the game
    void $ forkIO $ forever $ atomically $ readTChan interactionChan >>= \i -> case i of
        AskingAction (IAskingAction pid age necards gs _) pub -> do
            addCallback is pid (askActionCallback age necards gs pub)
            askActionDialog pid age necards gs output
        AskingCard (IAskingCard pid _ necards gs msg) pub -> do
            addCallback is pid (askCardCallback necards gs msg pub)
            askCardDialog pid necards gs msg output
        SendingMessage (ISimpleMessage imsg gameid) pub -> do
            let m = case imsg of
                        BroadcastCom c -> (RGameChannel gameid, displayCommunication c)
                        PlayerCom pid c -> (RPlayer pid, displayCommunication c)
            writeTChan output m
            fulfillPub pub ()
    -- the part that listens to messages from the server
    void $ forkIO $ forever $ join $ atomically $ readTChan input >>= \i -> case i of
        (_,"") -> retn
        (src,txt) -> if T.head txt == '!'
                         then let (cmd:args) = T.words (T.tail txt)
                              in  case commands ^. at cmd of
                                      Just h -> h is src args output
                                      Nothing -> writeTChan output (RPlayer src, "Invalid command!") >> retn
                         else do
                            m <- readTVar (_callbacks is)
                            case m ^. at src of
                                Just h -> h is src (T.words txt) output
                                Nothing -> retn

    return $ Backend desc backid interactionChan (terminateGame is)
