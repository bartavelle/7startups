{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Startups.Base
import Startups.Cards
import Startups.CardList
import Startups.GameTypes
import Startups.Utils
import Startups.Exported
import Backends.GenericHub
import Backends.Pure
import Strategies.Random

import Control.Lens
import Data.List (foldl')
import Test.Hspec
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import System.Random
import Test.QuickCheck
import Data.Monoid
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans.Except
import Data.Maybe (fromJust)

getCard :: T.Text -> Card
getCard n = case filter (\c -> c ^? cName == Just n) allcards of
                (c:_) -> c
                [] -> error (T.unpack n <> " could not be found")

-- | Some game state that is good enough for testing things
testState :: GameState
testState = GameState (M.fromList players) [] (mkStdGen 5)
    where
        players = [ ("pim", pim), ("pam", pam), ("poum", poum), ("bob", bob )]
        ppim = CompanyProfile Facebook A
        ppam = CompanyProfile Apple B
        ppoum = CompanyProfile Google A
        pbob = CompanyProfile Twitter A
        pim  = PlayerState ppim Project pimcards 1 ("pam", "bob") []
        pam  = PlayerState ppam Project pamcards 3 ("poum", "pim") []
        poum = PlayerState ppoum Project poumcards 6 ("bob", "pam") []
        bob  = PlayerState pbob Project bobcards 5 ("pim", "poum") []
        pimcards = map (getResourceCard ppim) [Project .. Stage1] <> map getCard [ "Cloud Servers"
                                                                                 , "Marketroid"
                                                                                 , "Company Nerf Battles"
                                                                                 ]
        pamcards = map (getResourceCard ppam) [Project] <> map getCard [ "High Speed Internet"
                                                                       , "Free Food"
                                                                       , "Enterprise Programmer"
                                                                       , "Rock Star Evangelist"
                                                                       ]
        poumcards = map (getResourceCard ppoum) [Project .. Stage1] <> map getCard [ "Garage"
                                                                                   , "Business Angel"
                                                                                   , "Admin Network"
                                                                                   ]
        bobcards = map (getResourceCard pbob) [Project] <> map getCard [ "Accountant"
                                                                       , "Operations Guru"
                                                                       , "Financial Developer"
                                                                       , "Standing Desks"
                                                                       ]

newtype HubWriter a = HubWriter { getHubWriter :: Writer [(GameId, GameEvent)] a }
                      deriving (Functor, Applicative, Monad, MonadWriter [(GameId, GameEvent)])

instance HubMonad HubWriter where
    getRand = return (mkStdGen 42)
    tellEvent gid e = tell [(gid, e)]

main :: IO ()
main = hspec $ do
    describe "Cards" $ do
        it "are all distinct" $ let extra = foldl' findExtra ([], S.empty) allcards
                                    findExtra (curlst, cardset) card | card `S.member` cardset = (card : curlst, cardset)
                                                                     | otherwise = (curlst, S.insert card cardset)
                                in  fst extra `shouldBe` []

        let nbc age nbplayers = it ("are the correct number for " ++ show age ++ " and " ++ show (getPlayerCount nbplayers) ++ " players") (cardsCount age nbplayers `shouldBe` expectedCount age nbplayers)
            expectedCount age nbplayers = fromIntegral $ nbplayers * 7 - if age == Age3 then nbplayers + 2 else 0
            cardsCount    age nbplayers = length (filter (\c -> c ^? cAge == Just age && c ^? cMinplayers <= Just nbplayers) allcards)
        mapM_ (uncurry nbc) [ (age, nbp) | age <- [Age1,Age2,Age3], nbp <- [3 .. 7] ]
    describe "availableResources" $
        forM_ [("pam", ["AVD$$$"]), ("pim", ["YMF$"]), ("poum", ["D$$$$$$"]), ("bob", ["YF$$$$$DM", "YF$$$$$FO", "YF$$$$$DO", "YF$$$$$FM"])] $ \(pid, reslist) ->
            let getResCost (Cost rescost _) = rescost
                expected = S.fromList (map getResCost reslist)
                actual = S.fromList $ availableResources OwnRes (fromJust (testState ^? playermap . ix pid))
            in  it ("Is correct for " <> T.unpack pid) $ actual `shouldBe` expected
    describe "random games" $ do
        let gs = do
                seed <- arbitrary
                nbplayers <- Test.QuickCheck.elements [3 .. 7]
                return (seed, nbplayers :: Int)
        it "end well" $ forAll gs $ \(seed, nbplayers) -> case pureGame stdGenStateStrat (mkStdGen seed) (map (T.pack . show) [1 .. nbplayers]) of
                                                              (_, Right _) -> True
                                                              _ -> False

    describe "Hub" $ do
        let mhs = initialHubstate
            runHubE :: ExceptT PlayerError HubWriter a -> (Either PlayerError a, [(GameId, GameEvent)])
            runHubE = runWriter . getHubWriter . runExceptT
            runHub :: HubWriter a -> (a, [(GameId, GameEvent)])
            runHub = runWriter . getHubWriter
            shouldGiveError act err = case runHubE act of
                                          (Left rr, _) -> rr `shouldBe` err
                                          _ -> fail "Should have failed"
        it "Should start with an empty game" $ games mhs `shouldBe` mempty
        it "Should not enter nonexistent games" $ joinGame mhs "bob" 12 `shouldGiveError` GameNotFound
        let ((gid, hs1),msgs) = runHub (newGame mhs "bob")
        it "Should create a new game" $ do
            gid `shouldBe` 0
            msgs `shouldBe` [(0,GameCreated)]
            games hs1 `shouldBe` M.singleton 0 (Joining (M.singleton "bob" Joined))
        let (res2, msgs2) = runHubE (joinGame hs1 "garry" 0 >>= \hs' -> joinGame hs' "john" 0)
            Right hs2 = res2
        it "Should register other players" $ do
            case res2 of
                Left rr -> fail (show rr)
                Right _ -> return ()
            msgs2 `shouldBe` [(0, PlayerJoinedGame "garry"), (0, PlayerJoinedGame "john")]
            games hs2 `shouldBe` M.singleton 0 (Joining (M.fromList [("bob", Joined), ("garry", Joined), ("john", Joined)]))
        let (res3, msgs3) = runHubE $ do
                (_, hsa) <- toggleReady hs2 0 "bob"
                (_, hsb) <- toggleReady hsa 0 "garry"
                (_, hsc) <- toggleReady hsb 0 "john"
                return hsc
            Right hs3 = res3
        it "Should toggle status until game starts" $ do
            msgs3 `shouldBe` [ (0, PlayerReady "bob" Ready)
                             , (0, PlayerReady "garry" Ready)
                             , (0, PlayerReady "john" Ready)
                             , (0, GameStarted ["bob","garry","john"])
                             , (0, PlayerMustPlay "bob")
                             , (0, PlayerMustPlay "garry")
                             , (0, PlayerMustPlay "john")
                             ]
        it "Should start the game properly" $ do
            let InGame gid' _ todo messages = playerStatus hs3 "bob"
                TodoAction age turn pid cards = todo
            gid' `shouldBe` 0
            todo `shouldSatisfy` has _TodoAction
            age `shouldBe` Age1
            turn `shouldBe` 1
            pid `shouldBe` "bob"
            length cards `shouldBe` 7
            messages `shouldBe` []
        let (res4, msgs4) = runHubE $ do
                InGame _ _ (TodoAction _ _ _ cardsBob) _ <- return (playerStatus hs3 "bob")
                InGame _ _ (TodoAction _ _ _ cardsGarry) _ <- return (playerStatus hs3 "garry")
                InGame _ _ (TodoAction _ _ _ cardsJohn) _ <- return (playerStatus hs3 "john")
                hsa <- playAction (PlayerAction Drop (head cardsBob)) mempty hs3 0 "bob"
                hsb <- playAction (PlayerAction Drop (head cardsGarry)) mempty hsa 0 "garry"
                hsc <- playAction (PlayerAction Drop (head cardsJohn)) mempty hsb 0 "john"
                return hsc
            Right hs4 = res4
        it "Should be possible to play" $ do
            res4 `shouldSatisfy` has _Right
            let InGame gid' _ todo messages = playerStatus hs4 "bob"
                TodoAction age turn pid cards = todo
            gid' `shouldBe` 0
            todo `shouldSatisfy` has _TodoAction
            age `shouldBe` Age1
            turn `shouldBe` 2
            pid `shouldBe` "bob"
            length cards `shouldBe` 6
            messages `shouldBe` []
