{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Backends.GenericHub
    ( GameId
    , PromiseId
    , HubState
    , PlayerError(..)
    , GameS
    , extractGameSummary
    , games
    , game
    , joinGame
    ) where

import Backends.Coroutine

import Startups.Base
import Startups.Cards
import Startups.Game
import Startups.GameTypes
import Startups.Utils
import Startups.Exported

import Data.Aeson
import Control.Concurrent.STM
import Control.Monad.State.Strict
import Control.Monad.Trans
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Random
import Control.Lens

newtype GameId = GameId { _getGameId :: Integer }
                 deriving (Show, Eq, Ord, Enum, Num, FromJSON, ToJSON)

newtype PromiseId = PromiseId { _getPromiseId :: Integer }
                    deriving (Show, Eq, Ord, Enum, Num, FromJSON, ToJSON)

newtype Promise x = Promise { _getPromise :: PromiseId }

newtype HubState = HubState { getHubState :: M.Map GameId GameS }

data GameS = GameJoining (M.Map PlayerId PlayerJoining)
           | GameOver (Either Message (M.Map PlayerId (M.Map VictoryType VictoryPoint)))
           | GamePlaying GP

data GP = GP { _gpPlayers  :: M.Map PlayerId PlayerMessages
             , _gpMaxprom  :: PromiseId
             , _gpBlocking :: Maybe (PromiseId, Int)
             , _gpGS       :: GameState
             }

data PlayerMessages = PlayerMessages { _curBlocking :: Maybe (Com Maybe)
                                     , _playerLog   :: [Message]
                                     }

class Monad m => HubMonad m where
    getRand :: m StdGen

defPlayerMessages :: PlayerMessages
defPlayerMessages = PlayerMessages Nothing []

data PlayerError = AlreadyPlaying
                 | GameAlreadyStarted
                 | GameFinished
                 | GameNotFound
                 | PlayerNotInGame
                 | CantPlayNow
                 deriving (Show, Eq, Read, Enum, Ord, Bounded)

makePrisms ''GameS
makeLenses ''GP
makeLenses ''PlayerMessages

_GamePlayers :: Traversal' GameS (S.Set PlayerId)
_GamePlayers f s = case s of
                       GameJoining m               -> fmap (GameJoining . rebuildMap Joined m) (f (M.keysSet m))
                       GamePlaying (GP m mp bl gs) -> fmap (\st -> GamePlaying (GP (rebuildMap defPlayerMessages m st) mp bl gs)) (f (M.keysSet m))
                       GameOver (Left _)           -> pure s
                       GameOver (Right _)          -> pure s
    where
        rebuildMap :: a -> M.Map PlayerId a -> S.Set PlayerId -> M.Map PlayerId a
        rebuildMap def mp userset = M.filterWithKey (\k _ -> k `S.member` userset) mp `M.union` M.fromSet (const def) userset


extractGameSummary :: GameS -> GameSummary
extractGameSummary gs = case gs of
                            GameJoining m -> Joining m
                            GameOver o -> Finished (fmap (fmap VictoryMap) o)
                            GamePlaying (GP pm _ _ gs') -> Started (exportGameState gs') $ do
                                (pid, PlayerMessages blk msgs) <- itoList pm
                                let activity = maybe Waiting (const Playing) blk
                                return (pid, activity, msgs)

games :: HubState -> M.Map GameId GameSummary
games = fmap extractGameSummary . getHubState

game :: HubState -> GameId -> Maybe GameSummary
game (HubState hs) gid = fmap extractGameSummary (M.lookup gid hs)

playerGame :: HubState -> PlayerId -> Maybe (GameId, GameSummary)
playerGame (HubState hs) pid = case M.toList (M.filter (has (_GamePlayers . ix pid)) hs) of
                                   (x : _) -> Just (fmap extractGameSummary x)
                                   _ -> Nothing

joinGame :: HubMonad m => HubState -> PlayerId -> GameId -> ExceptT PlayerError m HubState
joinGame nhs@(HubState hs) pid gid = do
    case playerGame nhs pid of
        Nothing -> return ()
        Just _ -> throwE AlreadyPlaying
    case hs ^? ix gid of
        Nothing -> throwE GameNotFound
        Just GamePlaying{} -> throwE GameAlreadyStarted
        Just (GameOver _) -> throwE GameFinished
        Just (GameJoining _) -> lift $ checkGameStart (HubState (hs & ix gid . _GameJoining . at pid ?~ Joined)) gid

checkGameStart :: HubMonad m => HubState -> GameId -> m HubState
checkGameStart nhs@(HubState hs) gid =
    case hs ^? ix gid . _GameJoining of
        Nothing -> return nhs
        Just mp -> if M.size mp >= 7 || (all (== Ready) mp && M.size mp > 1)
                       then startGame nhs gid (M.keysSet mp)
                       else return nhs


startGame :: HubMonad m => HubState -> GameId -> S.Set PlayerId -> m HubState
startGame (HubState hs) gid players = do
    rgen <- getRand
    let gs = initialGameState rgen (S.toList players)
        gp = GP (M.fromSet (const defPlayerMessages) players) 1 Nothing gs
        x = advanceGame gp gs playGame
    undefined

-- advanceGame :: GameState -> GameMonad Maybe a -> Either (Y (Com Maybe) 
advanceGame gp gs act = runState (resume $ runCR getPromise mkPromise gs act) gp
    where
        getPromise :: Promise x -> State GP x
        getPromise = undefined
        mkPromise :: State GP (Promise x)
        mkPromise = fmap Promise (gpMaxprom <+= 1)
