{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Useful for safely exporting data without exposing hidden information
module Startups.Exported where

import qualified Data.Map.Strict as M
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Control.Lens

import Startups.Json
import Startups.Base
import Startups.Cards
import Startups.GameTypes

data ExportedPlayerState = ExportedPlayerState { _eCompany         :: CompanyProfile
                                               , _eCompanyStage    :: CompanyStage
                                               , _eCardsCount      :: Int
                                               , _eFunds           :: Funding
                                               , _eNeighborhood    :: Neighborhood
                                               , _ePoachingResults :: [PoachingOutcome]
                                               } deriving (Eq, Show)

data ExportedGameState = ExportedGameState { _eplayermap   :: M.Map PlayerId ExportedPlayerState
                                           , _ediscardSize :: Int
                                           } deriving (Eq, Show)

newtype GameId = GameId { _getGameId :: Integer }
                 deriving (Show, Eq, Ord, Enum, Num, FromJSON, ToJSON)

data PlayerStatus = Inactive
                  | InGame GameId GameSummary Todo [Message]
                  deriving (Show, Eq)

data Todo = TodoAction Age Turn PlayerId [Card]
          | TodoCard   Age PlayerId [Card] Message
          | TodoNothing
          deriving (Show, Eq)

exportGameState :: GameState -> ExportedGameState
exportGameState gs = ExportedGameState (fmap exportPlayerState (_playermap gs)) (length (_discardpile gs))
    where
        exportPlayerState pm = ExportedPlayerState (_pCompany pm)
                                                   (_pCompanyStage pm)
                                                   (length (_pCards pm))
                                                   (_pFunds pm)
                                                   (_pNeighborhood pm)
                                                   (_pPoachingResults pm)

newtype VictoryMap = VictoryMap { getVictoryMap :: M.Map VictoryType VictoryPoint }
                     deriving (Eq, Show)

instance ToJSON VictoryMap where
    toJSON = toJSON . M.fromList . (traverse . _1 %~ show) . M.toList . getVictoryMap

instance FromJSON VictoryMap where
    parseJSON = withObject "VictoryMap" (fmap (VictoryMap . M.fromList) . mapM readKey . itoList)
        where
            readKey :: (Text, Value) -> Parser (VictoryType, VictoryPoint)
            readKey (k,v) = (,) <$> parseJSON (String k) <*> parseJSON v

data PlayerJoining = Joined | Ready
                   deriving (Show, Eq, Enum, Bounded)

data GameSummary = Joining (M.Map PlayerId PlayerJoining)
                 | Zombie [PlayerId]
                 | Started ExportedGameState [(PlayerId, PlayerActivity, [Message])]
                 | Finished (Either Message (M.Map PlayerId VictoryMap))
                 deriving (Eq, Show)

data PlayerActivity = Waiting | Playing
                    deriving (Show, Eq, Enum, Bounded)

data PlayerError = AlreadyPlaying
                 | GameAlreadyStarted
                 | GameFinished
                 | GameNotFound
                 | PlayerNotInGame
                 | CantPlayNow
                 deriving (Show, Eq, Read, Enum, Ord, Bounded)

data GameEvent = PlayerJoinedGame PlayerId
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
$(deriveJSON baseOptions ''PlayerStatus)
$(deriveJSON baseOptions ''Todo)
$(deriveJSON baseOptions ''GameSummary)
$(deriveJSON baseOptions ''PlayerJoining)
$(deriveJSON baseOptions ''PlayerActivity)
$(deriveJSON baseOptions ''PlayerError)
$(deriveJSON baseOptions ''GameEvent)
$(deriveJSON (dropOptions 2) ''ExportedGameState)
$(deriveJSON (dropOptions 2) ''ExportedPlayerState)

