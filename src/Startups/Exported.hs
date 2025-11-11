{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Useful for safely exporting data without exposing hidden information
module Startups.Exported where

import Control.Lens
import Data.Aeson hiding (defaultOptions)
import qualified Data.Map.Strict as M
import Elm.Derive
import Startups.Base
import Startups.Cards
import Startups.Game (victoryPoints')
import Startups.GameTypes
import Startups.Json

data ExportedPlayerState = ExportedPlayerState
  { _eCompany :: CompanyProfile,
    _eCompanyStage :: CompanyStage,
    _eCards :: [Card],
    _eFunds :: Funding,
    _eNeighborhood :: Neighborhood,
    _ePoachingResults :: [PoachingOutcome],
    _eVictory :: VictoryMap
  }
  deriving (Eq, Show)

data ExportedGameState = ExportedGameState
  { _eplayermap :: M.Map PlayerId ExportedPlayerState,
    _ediscardSize :: Int
  }
  deriving (Eq, Show)

newtype GameId = GameId {_getGameId :: Integer}
  deriving (Show, Eq, Ord, Enum, Num, FromJSON, ToJSON)

data PlayerStatus
  = Inactive
  | InGame GameId GameSummary Todo [Message]
  deriving (Show, Eq)

data Todo
  = TodoAction Age Turn PlayerId [(Card, Funding, VictoryPoint)] [(PlayerAction, Exchange, Maybe SpecialInformation)]
  | TodoCard Age PlayerId [(Card, Funding, VictoryPoint)] Message
  | TodoNothing
  deriving (Show, Eq)

exportGameState :: GameState -> ExportedGameState
exportGameState gs =
  ExportedGameState (M.mapWithKey exportPlayerState stt) (length (_discardpile gs))
  where
    stt = _playermap gs
    vpoints = victoryPoints' stt
    exportPlayerState playername pm =
      ExportedPlayerState
        (_pCompany pm)
        (_pCompanyStage pm)
        (_pCards pm)
        (_pFunds pm)
        (_pNeighborhood pm)
        (_pPoachingResults pm)
        (VictoryMap (vpoints ^. ix playername))

newtype VictoryMap = VictoryMap {getVictoryMap :: M.Map VictoryType VictoryPoint}
  deriving (Eq, Show)

instance ToJSON VictoryMap where
  toJSON = toJSON . M.toList . getVictoryMap

instance FromJSON VictoryMap where
  parseJSON = fmap (VictoryMap . M.fromList) . parseJSON

data PlayerJoining = Joined | Ready
  deriving (Show, Eq, Enum, Bounded)

data GameSummary
  = Joining (M.Map PlayerId PlayerJoining)
  | Zombie [PlayerId]
  | Started ExportedGameState [(PlayerId, PlayerActivity, [Message])]
  | FinishedBad Message
  | Finished (M.Map PlayerId VictoryMap)
  deriving (Eq, Show)

data PlayerActivity = Waiting | Playing
  deriving (Show, Eq, Enum, Bounded)

data PlayerError
  = AlreadyPlaying
  | GameAlreadyStarted
  | GameFinished
  | GameNotFound
  | PlayerNotInGame
  | CantPlayNow
  | NotAuthorized
  | Other String
  deriving (Show, Eq, Read, Ord)

data GameEvent
  = PlayerJoinedGame PlayerId
  | PlayerLeftGame PlayerId
  | GameCreated
  | GameStarted [PlayerId]
  | PlayerMustPlay PlayerId
  | PlayerReady PlayerId PlayerJoining
  | PCom PlayerId Message
  | BCom Message
  deriving (Show, Eq)

makePrisms ''GameSummary
makePrisms ''PlayerStatus
makePrisms ''Todo
makeLenses ''ExportedGameState
makeLenses ''ExportedPlayerState
$(deriveBoth baseOptions ''Todo)
$(deriveBoth baseOptions ''PlayerJoining)
$(deriveBoth (dropOptions 2) ''ExportedPlayerState)
$(deriveBoth (dropOptions 2) ''ExportedGameState)
$(deriveBoth baseOptions ''PlayerActivity)
$(deriveBoth baseOptions ''GameSummary)
$(deriveBoth baseOptions ''PlayerStatus)
$(deriveBoth baseOptions ''PlayerError)
$(deriveBoth baseOptions ''GameEvent)
$(deriveElmDef baseOptions ''VictoryMap)
