{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
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

import Startups.Base
import Startups.Cards
import Startups.Game
import Startups.GameTypes
import Startups.Utils
import Startups.Exported

import Data.Aeson hiding ((.=))
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Operational
import Data.List.NonEmpty
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Random
import Control.Lens

type GameResult = M.Map PlayerId (M.Map VictoryType VictoryPoint)

newtype GameId = GameId { _getGameId :: Integer }
                 deriving (Show, Eq, Ord, Enum, Num, FromJSON, ToJSON)

newtype PromiseId = PromiseId { _getPromiseId :: Integer }
                    deriving (Show, Eq, Ord, Enum, Num, FromJSON, ToJSON)

newtype Promise x = Promise { _getPromise :: PromiseId }
                    deriving (Show, Eq, Ord, Enum, Num, FromJSON, ToJSON)

newtype HubState = HubState { getHubState :: M.Map GameId GameS }

data GameS = GameJoining (M.Map PlayerId PlayerJoining)
           | GameOver (Either Message GameResult)
           | GamePlaying GP

data GP = GP { _gpPlayers  :: M.Map PlayerId PlayerMessages
             , _gpMaxprom  :: PromiseId
             , _gpBlocking :: BlockingOn
             , _gpCardProm :: M.Map (Promise Card) Card
             , _gpActProm  :: M.Map (Promise (PlayerAction, Exchange)) (PlayerAction, Exchange)
             , _gpGS       :: GameState
             }

data BlockingOn = NotBlocked
                | BlockingOnCard   GameState (Promise Card)           (Card -> GameMonad Promise GameResult)
                | BlockingOnAction GameState (Promise (PlayerAction, Exchange)) ((PlayerAction, Exchange) -> GameMonad Promise GameResult)

data AP = AP Age Turn PlayerId (NonEmpty Card) GameState
data AC = AC Age PlayerId (NonEmpty Card) GameState Message

data Com = CAP AP (Promise (PlayerAction, Exchange))
         | CAC AC (Promise Card)

data PlayerMessages = PlayerMessages { _curBlocking :: Maybe Com
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
                       GamePlaying gp              -> let m = gp ^. gpPlayers
                                                      in  fmap (\st -> GamePlaying (gp & gpPlayers .~ rebuildMap defPlayerMessages m st)) (f (M.keysSet m))
                       GameOver (Left _)           -> pure s
                       GameOver (Right _)          -> pure s
    where
        rebuildMap :: a -> M.Map PlayerId a -> S.Set PlayerId -> M.Map PlayerId a
        rebuildMap def mp userset = M.filterWithKey (\k _ -> k `S.member` userset) mp `M.union` M.fromSet (const def) userset


extractGameSummary :: GameS -> GameSummary
extractGameSummary gs = case gs of
                            GameJoining m -> Joining m
                            GameOver o -> Finished (fmap (fmap VictoryMap) o)
                            GamePlaying (GP pm _ _ _ _ gs') -> Started (exportGameState gs') $ do
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
        gp = GP (M.fromSet (const defPlayerMessages) players) 1 NotBlocked M.empty M.empty gs
    return $ HubState (hs & at gid ?~ advanceGame gp gs playGame)

advanceGame :: GP -> GameState -> GameMonad Promise GameResult -> GameS
advanceGame gp gs act = case step gp gs act of
                            Fin x -> GameOver (Right x)
                            Failed rr -> GameOver (Left rr)
                            GPA gp' gs' prom a -> GamePlaying (gp' & gpBlocking .~ BlockingOnAction gs' prom a)
                            GPC gp' gs' prom a -> GamePlaying (gp' & gpBlocking .~ BlockingOnCard gs' prom a)

data StepResult a = GPA GP GameState (Promise (PlayerAction, Exchange)) ((PlayerAction, Exchange) -> GameMonad Promise a)
                  | GPC GP GameState (Promise Card) (Card -> GameMonad Promise a)
                  | Fin a
                  | Failed Message

step :: GP -> GameState -> GameMonad Promise a -> StepResult a
step initialgp gs act = case r of
                     Return x -> Fin x
                     a :>>= f -> tst a f
    where
        (r, gs') = runState (viewT act) gs
        tst :: GameInstr Promise b -> (b -> ProgramT (GameInstr Promise) (StateT GameState Identity) a) -> StepResult a
        tst a f =
            let mkpromise :: (PromiseId, GP)
                mkpromise = gp & gpMaxprom <%~ succ
                gp = initialgp & gpGS .~ gs'
            in  case a of
                    GetPromiseCard pc -> case gp ^? gpCardProm . ix pc of
                                             Just card -> step gp gs' (f card)
                                             Nothing -> GPC gp gs' pc f
                    GetPromiseAct pa  -> case gp ^? gpActProm . ix pa of
                                            Just action -> step gp gs' (f action)
                                            Nothing -> GPA gp gs' pa f
                    PlayerDecision age turn pid clist -> do
                        let apa = AP age turn pid clist gs'
                            (promid, gp') = mkpromise
                            prom = Promise promid
                            gp'' = gp' & gpPlayers . ix pid . curBlocking ?~ CAP apa prom
                        step gp'' gs' (f prom)
                    AskCard age pid cards msg -> do
                        let apc = AC age pid cards gs' msg
                            (promid, gp') = mkpromise
                            prom = Promise promid
                            gp'' = gp' & gpPlayers . ix pid . curBlocking ?~ CAC apc prom
                        step gp'' gs' (f prom)
                    Message com -> let gp' = case com of
                                           PlayerCom pid (RawMessage msg) -> gp & gpPlayers . ix pid . playerLog %~ (msg :)
                                           BroadcastCom (RawMessage msg) -> gp & gpPlayers . traverse . playerLog %~ (msg :)
                                           _ -> gp
                                   in  step gp' gs' (f ())
                    ThrowError err -> Failed err
                    CatchError n handler -> case step gp gs' n of
                                                Failed rr -> step gp gs' (handler rr >>= f)
                                                Fin x -> step gp gs' (f x)
                                                GPA _ _ _ _ -> Failed "Can't catch error when asking for a promise in the middle"
                                                GPC _ _ _ _ -> Failed "Can't catch error when asking for a promise in the middle"
