{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Backends.GenericHub
    ( GameId
    , PromiseId
    , HubState
    , HubMonad(..)
    , GameS
    , initialHubstate
    , extractGameSummary
    , games
    , game
    , playerStatus
    , newGame
    , joinGame
    , leaveGame
    , toggleReady
    , playCard
    , playAction
    , PureHub
    , runPureHub
    ) where

import Startups.Base
import Startups.Cards
import Startups.Game hiding (playCard)
import Startups.GameTypes
import Startups.Utils
import Startups.Exported

import Data.Aeson hiding ((.=))
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Operational
import Control.Monad.RSS.Strict
import Data.List.NonEmpty
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Random
import Control.Lens

type GameResult = M.Map PlayerId (M.Map VictoryType VictoryPoint)

newtype PromiseId = PromiseId { _getPromiseId :: Integer }
                    deriving (Show, Eq, Ord, Enum, Num, FromJSON, ToJSON)

newtype Promise x = Promise { _getPromise :: PromiseId }
                    deriving (Show, Eq, Ord, Enum, Num, FromJSON, ToJSON)

newtype HubState = HubState { getHubState :: M.Map GameId GameS }

data GameS = GameJoining (M.Map PlayerId PlayerJoining)
           | GameOver (Either Message GameResult) [ActionRecap]
           | GamePlaying GP

data GP = GP { _gpPlayers  :: M.Map PlayerId PlayerMessages
             , _gpMaxprom  :: PromiseId
             , _gpBlocking :: BlockingOn
             , _gpCardProm :: M.Map (Promise Card) Card
             , _gpActProm  :: M.Map (Promise (PlayerAction, Exchange, Maybe SpecialInformation)) (PlayerAction, Exchange, Maybe SpecialInformation)
             , _gpGS       :: GameState
             , _gpLog      :: [ActionRecap]
             }

data BlockingOn = NotBlocked
                | BlockingOnCard   GameState (Promise Card)                     (Card -> GameMonad Promise GameResult)
                | BlockingOnAction GameState (Promise (PlayerAction, Exchange, Maybe SpecialInformation)) ((PlayerAction, Exchange, Maybe SpecialInformation) -> GameMonad Promise GameResult)

data AP = AP Age Turn PlayerId (NonEmpty Card) GameState
data AC = AC Age PlayerId (NonEmpty Card) GameState Message

data Com = CAP AP (Promise (PlayerAction, Exchange, Maybe SpecialInformation))
         | CAC AC (Promise Card)

data PlayerMessages = PlayerMessages { _curBlocking :: Maybe Com
                                     , _playerLog   :: [Message]
                                     }

class Monad m => HubMonad m where
    getRand   :: m StdGen
    tellEvent :: GameId -> GameEvent -> m ()
    gamelog   :: GameId -> ActionRecap -> m ()

defPlayerMessages :: PlayerMessages
defPlayerMessages = PlayerMessages Nothing []

makePrisms ''GameS
makePrisms ''Com
makePrisms ''BlockingOn
makeLenses ''GP
makeLenses ''PlayerMessages
makeWrapped ''HubState

type GTrav x = Traversal' (M.Map GameId GameS) x

newtype PureHub time a = PureHub { getPureHub :: RSST (time, StdGen) [(time, GameId, GameEvent)] HubState (Except PlayerError) a }
                         deriving (Functor, Applicative, Monad)

runPureHub :: PureHub time a -> time -> StdGen -> HubState -> Either PlayerError (a, HubState, [(time, GameId, GameEvent)])
runPureHub a time stdgen hs = runExcept (runRSST (getPureHub a) (time, stdgen) hs)

instance MonadWriter [(time, GameId, GameEvent)] (PureHub time) where
  tell = PureHub . tell
  listen = PureHub . listen . getPureHub
  pass = PureHub . pass . getPureHub

instance MonadReader (time, StdGen) (PureHub time) where
  ask = PureHub ask
  local f m = PureHub (local f (getPureHub m))

instance MonadState HubState (PureHub time) where
  state f = PureHub (state f)

instance MonadError PlayerError (PureHub time) where
  throwError = PureHub . throwError
  catchError e h = PureHub (catchError (getPureHub e) (getPureHub . h))

instance HubMonad (PureHub time) where
  getRand = asks snd
  tellEvent gid event = do
    now <- asks fst
    tell [(now,gid,event)]
  gamelog gid ar = _Wrapped' . ix gid . _GamePlaying . gpLog %= (ar :)

zoomHub :: (MonadError PlayerError m, HubMonad m, MonadState HubState m) => GTrav a -> PlayerError -> (a -> m a) -> m ()
zoomHub trav rr a = do
    HubState hs <- get
    case hs ^? trav of
        Nothing -> throwError rr
        Just x -> do
            x' <- a x
            put (HubState (hs & trav .~ x'))

withGame :: (MonadError PlayerError m, HubMonad m, MonadState HubState m) => GameId -> (GameS -> m GameS) -> m ()
withGame gid = zoomHub (ix gid) CantPlayNow

_GamePlayers :: Traversal' GameS (S.Set PlayerId)
_GamePlayers f s = case s of
                       GameJoining m  -> fmap (GameJoining . rebuildMap Joined m) (f (M.keysSet m))
                       GamePlaying gp -> let m = gp ^. gpPlayers
                                         in  fmap (\st -> GamePlaying (gp & gpPlayers .~ rebuildMap defPlayerMessages m st)) (f (M.keysSet m))
                       GameOver _ _   -> pure s
    where
        rebuildMap :: a -> M.Map PlayerId a -> S.Set PlayerId -> M.Map PlayerId a
        rebuildMap def mp userset = M.filterWithKey (\k _ -> k `S.member` userset) mp `M.union` M.fromSet (const def) userset

convertMCom :: Maybe Com -> Todo
convertMCom mc = case mc of
                     Nothing -> TodoNothing
                     Just (CAP (AP age turn pid cards gs) _) -> TodoAction age turn pid (withEffects pid (gs ^. playermap) <$> toList cards) (toList (allowableActions age pid cards (gs ^. playermap)))
                     Just (CAC (AC age pid cards gs msg) _)  -> TodoCard   age pid (withEffects pid (gs ^. playermap) <$> toList cards) msg
  where withEffects pid pmap c =
          let (f,v) = cardEffectPreview pid c pmap
          in  (c,f,v)

extractGameSummary :: GameS -> GameSummary
extractGameSummary gs = case gs of
                            GameJoining m -> Joining m
                            GameOver o _ -> either FinishedBad (Finished . fmap VictoryMap) o
                            GamePlaying (GP pm _ _ _ _ gs' _) -> Started (exportGameState gs') $ do
                                (pid, PlayerMessages blk msgs) <- itoList pm
                                let activity = maybe Waiting (const Playing) blk
                                return (pid, activity, msgs)

initialHubstate :: HubState
initialHubstate = HubState mempty

games :: HubState -> M.Map GameId GameSummary
games = fmap extractGameSummary . getHubState

game :: HubState -> GameId -> Maybe GameSummary
game (HubState hs) gid = fmap extractGameSummary (M.lookup gid hs)

playerGame :: HubState -> PlayerId -> Maybe (GameId, GameSummary)
playerGame (HubState hs) pid = case M.toList (M.filter (has (_GamePlayers . ix pid)) hs) of
                                   (x : _) -> Just (fmap extractGameSummary x)
                                   _ -> Nothing

playerStatus :: HubState -> PlayerId -> PlayerStatus
playerStatus (HubState hs) pid =
    case M.toList (M.filter (has (_GamePlayers . ix pid)) hs) of
        ( (gid, gameS) : _ ) ->
            let (todo, messages) = case gameS ^? _GamePlaying . gpPlayers . ix pid of
                                       Nothing -> (TodoNothing, [])
                                       Just (PlayerMessages mcom msgs) -> (convertMCom mcom, msgs)
            in  InGame gid (extractGameSummary gameS) todo messages
        [] -> Inactive

newGame :: (MonadError PlayerError m, HubMonad m, MonadState HubState m) => PlayerId -> m GameId
newGame pid = do
    HubState hs <- get
    case playerGame (HubState hs) pid of
        Nothing -> return ()
        Just _ -> throwError AlreadyPlaying
    let gid = maybe 0 (succ . fst . fst) (M.maxViewWithKey hs)
    tellEvent gid GameCreated
    _Wrapped' . at gid ?= GameJoining (M.singleton pid Joined)
    return gid

joinGame :: (MonadError PlayerError m, HubMonad m, MonadState HubState m) => PlayerId -> GameId -> m Bool
joinGame pid gid = do
    nhs@(HubState hs) <- get
    case playerGame nhs pid of
        Nothing -> return ()
        Just _ -> throwError AlreadyPlaying
    case hs ^? ix gid of
        Nothing -> throwError GameNotFound
        Just GamePlaying{} -> throwError GameAlreadyStarted
        Just (GameOver _ _) -> throwError GameFinished
        Just (GameJoining _) -> do
            tellEvent gid (PlayerJoinedGame pid)
            _Wrapped' . ix gid . _GameJoining . at pid ?= Joined
            checkGameStart gid

leaveGame :: (MonadError PlayerError m, HubMonad m, MonadState HubState m) => PlayerId -> GameId -> m ()
leaveGame pid gid = do
  nhs <- get
  case playerGame nhs pid of
      Nothing -> return ()
      Just (gid', Joining _) -> do
        unless (gid == gid') (throwError PlayerNotInGame)
        tellEvent gid (PlayerLeftGame pid)
        _Wrapped' . ix gid . _GameJoining . at pid .= Nothing
      Just _ -> throwError AlreadyPlaying

checkGameStart :: (MonadError PlayerError m, HubMonad m, MonadState HubState m) => GameId -> m Bool
checkGameStart gid = do
    gj <- preuse (_Wrapped' . ix gid . _GameJoining)
    case gj of
      Just mp | M.size mp >= 7 || (all (== Ready) mp && M.size mp > 2)
        -> True <$ startGame gid (M.keysSet mp)
      _ -> return False

startGame :: (MonadError PlayerError m, HubMonad m, MonadState HubState m) => GameId -> S.Set PlayerId -> m ()
startGame gid players = do
    tellEvent gid (GameStarted (S.toList players))
    rgen <- getRand
    let gs = initialGameState rgen (S.toList players)
        gp = GP (M.fromSet (const defPlayerMessages) players) 1 NotBlocked M.empty M.empty gs []
    gameS <- advanceGame gid gp gs playGame
    _Wrapped' . at gid ?= gameS

toggleReady :: (MonadError PlayerError m, HubMonad m, MonadState HubState m) => GameId -> PlayerId -> m PlayerJoining
toggleReady gid pid = do
    mgs <- preuse (_Wrapped' . ix gid)
    case mgs of
        Nothing -> throwError PlayerNotInGame
        Just (GameJoining mp) -> do
            let toggle s = do
                    tellEvent gid (PlayerReady pid s)
                    _Wrapped' . ix gid . _GameJoining . ix pid .= s
                    return s
            case mp ^? ix pid of
                Nothing -> throwError PlayerNotInGame -- should not happen
                Just Ready -> toggle Joined
                Just Joined -> toggle Ready <* checkGameStart gid
        Just GamePlaying{} -> throwError GameAlreadyStarted
        Just (GameOver _ _) -> throwError GameFinished

playCard :: (MonadError PlayerError m, HubMonad m, MonadState HubState m) => Card -> GameId -> PlayerId -> m ()
playCard = genPlay _BlockingOnCard gpCardProm (_CAC . _2)

playAction :: (MonadError PlayerError m, HubMonad m, MonadState HubState m) => PlayerAction -> Exchange -> Maybe SpecialInformation -> GameId -> PlayerId -> m ()
playAction pa e mspecial = genPlay _BlockingOnAction gpActProm (_CAP . _2) (pa, e, mspecial)

genPlay :: (MonadError PlayerError m, HubMonad m, MonadState HubState m)
        => Prism' BlockingOn (GameState, Promise toplay, toplay -> GameMonad Promise GameResult)
        -> Lens' GP (M.Map (Promise toplay) toplay)
        -> Traversal' Com (Promise toplay)
        -> toplay
        -> GameId
        -> PlayerId
        -> m ()
genPlay blockPrism promMap comPrism toplay gid pid = withGame gid $ \gameS ->
    case gameS ^? _GamePlaying of
        Nothing -> throwError CantPlayNow
        Just gp -> case gp ^? gpPlayers . ix pid . curBlocking . _Just . comPrism of
          Just prom -> do
              let gp' = gp & gpPlayers . ix pid . curBlocking .~ Nothing
                  finished = return $ GamePlaying $ gp' & promMap . at prom ?~ toplay
              case gp' ^? gpBlocking . blockPrism of
                  Just (gs, prom', act) ->
                      if prom' == prom
                          then advanceGame gid gp' gs (act toplay)
                          else finished
                  _ -> finished
          Nothing -> throwError CantPlayNow

-- | The entry point to run the game and update its state
advanceGame :: (MonadState HubState m, HubMonad m)
            => GameId
            -> GP
            -> GameState
            -> GameMonad Promise GameResult
            -> m GameS
advanceGame gid gp gs act = do
    s <- step gid gp gs act
    msgs <- use (_Wrapped' . ix gid . _GamePlaying . gpLog)
    return $ case s of
                 Fin x -> GameOver (Right x) msgs
                 Failed rr -> GameOver (Left rr) msgs
                 GPA gp' gs' prom a -> GamePlaying (gp' & gpBlocking .~ BlockingOnAction gs' prom a)
                 GPC gp' gs' prom a -> GamePlaying (gp' & gpBlocking .~ BlockingOnCard gs' prom a)

data StepResult a = GPA GP GameState (Promise (PlayerAction, Exchange, Maybe SpecialInformation)) ((PlayerAction, Exchange, Maybe SpecialInformation) -> GameMonad Promise a)
                  | GPC GP GameState (Promise Card) (Card -> GameMonad Promise a)
                  | Fin a
                  | Failed Message

step :: HubMonad m => GameId -> GP -> GameState -> GameMonad Promise a -> m (StepResult a)
step gid initialgp gs act = case r of
                                Return x -> return $ Fin x
                                a :>>= f -> tst a f
    where
        (r, gs') = runState (viewT act) gs
        tst :: HubMonad m => GameInstr Promise b -> (b -> ProgramT (GameInstr Promise) (StateT GameState Identity) a) -> m (StepResult a)
        tst a f =
            let mkpromise :: (PromiseId, GP)
                mkpromise = gp & gpMaxprom <%~ succ
                gp = initialgp & gpGS .~ gs'
            in  case a of
                    GetPromiseCard pc -> case gp ^? gpCardProm . ix pc of
                                             Just card -> step gid gp gs' (f card)
                                             Nothing -> return $ GPC gp gs' pc f
                    GetPromiseAct pa  -> case gp ^? gpActProm . ix pa of
                                            Just action -> step gid gp gs' (f action)
                                            Nothing -> return $ GPA gp gs' pa f
                    PlayerDecision age turn pid clist -> do
                        let apa = AP age turn pid clist gs'
                            (promid, gp') = mkpromise
                            prom = Promise promid
                            gp'' = gp' & gpPlayers . ix pid . curBlocking ?~ CAP apa prom
                        tellEvent gid (PlayerMustPlay pid)
                        step gid gp'' gs' (f prom)
                    AskCard age pid cards msg -> do
                        let apc = AC age pid cards gs' msg
                            (promid, gp') = mkpromise
                            prom = Promise promid
                            gp'' = gp' & gpPlayers . ix pid . curBlocking ?~ CAC apc prom
                        tellEvent gid (PlayerMustPlay pid)
                        step gid gp'' gs' (f prom)
                    Message com -> do
                        gp' <- case com of
                                   PlayerCom pid (RawMessage msg) -> do
                                       tellEvent gid (PCom pid msg)
                                       return $ gp & gpPlayers . ix pid . playerLog %~ (msg :)
                                   BroadcastCom (RawMessage msg) -> do
                                       tellEvent gid (BCom msg)
                                       return $ gp & gpPlayers . traverse . playerLog %~ (msg :)
                                   BroadcastCom (ActionRecapMsg recap) -> gp <$ gamelog gid recap
                                   _ -> return gp
                        step gid gp' gs' (f ())
                    ThrowError err -> return $ Failed err
                    CatchError n handler -> step gid gp gs' n >>= \y -> case y of
                                                Failed rr -> step gid gp gs' (handler rr >>= f)
                                                Fin x -> step gid gp gs' (f x)
                                                GPA _ _ _ _ -> return $ Failed "Can't catch error when asking for a promise in the middle"
                                                GPC _ _ _ _ -> return $ Failed "Can't catch error when asking for a promise in the middle"
