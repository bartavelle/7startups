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
import Data.Maybe (fromJust)
import Data.List.NonEmpty (NonEmpty(..))

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
        pimcards = map (getResourceCard ppim) [Project .. Stage1] <> map getCard [ "Private Cloud"
                                                                                 , "Marketroid"
                                                                                 , "Internet Legend"
                                                                                 ]
        pamcards = map (getResourceCard ppam) [Project] <> map getCard [ "High Speed Internet"
                                                                       , "Artisan Chef"
                                                                       , "Brogrammer"
                                                                       , "Benevolent Dictator"
                                                                       ]
        poumcards = map (getResourceCard ppoum) [Project .. Stage1] <> map getCard [ "Garage"
                                                                                   , "Business Angel"
                                                                                   , "Segmented Network"
                                                                                   ]
        bobcards = map (getResourceCard pbob) [Project] <> map getCard [ "Accountant"
                                                                       , "Commercial Engineer"
                                                                       , "High Frequency Trader"
                                                                       , "Fußball"
                                                                       ]

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
        forM_ [("pam", ["AVD$$$"]), ("pim", ["HMF$"]), ("poum", ["D$$$$$$"]), ("bob", ["HF$$$$$DM", "HF$$$$$FO", "HF$$$$$DO", "HF$$$$$FM"])] $ \(pid, reslist) ->
            let getResCost (Cost rescost _) = rescost
                expected = S.fromList (map getResCost reslist)
                actual = S.fromList $ availableResources OwnRes (fromJust (testState ^? playermap . ix pid))
            in  it ("Is correct for " <> T.unpack pid) $ actual `shouldBe` expected
    describe "Opportunity" $ do
      let cny = CompanyProfile Yahoo A
          heldCards = map (getResourceCard cny) [Project .. Stage3]
          p1s = PlayerState cny Stage3 heldCards 10 ("p3","p2") []
          p2s = p1s & pNeighborhood .~ ("p1","p3")
          p3s = p1s & pNeighborhood .~ ("p2","p1")
          market = getCard "Marketroid"
          lavish = getCard "Lavish Headquarters"
          devops = getCard "Devops Team"
          pmap = M.fromList (zip ["p1","p2","p3"] [p1s,p2s,p3s])
      it "Should only give the option to play marketroid in the first test" $
        allowableActions Age1 "p1" (market :| []) pmap `shouldBe`
             (PlayerAction Play market, mempty, Nothing) :| [ (PlayerAction Drop market, mempty, Nothing) ]
      it "Should only give the option to play the lavish headquarters with opportunity" $
        allowableActions Age1 "p1" (lavish :| []) pmap `shouldBe`
             (PlayerAction Play lavish, mempty, Just UseOpportunity) :| [ (PlayerAction Drop lavish, mempty, Nothing) ]
      it "Should only give the option to play the devops team with or without cost" $
        allowableActions Age1 "p1" (devops :| []) pmap `shouldBe`
             (PlayerAction Play devops, mempty, Nothing) :| [ (PlayerAction Play devops, mempty, Just UseOpportunity), (PlayerAction Drop devops, mempty, Nothing) ]
    describe "random games" $ do
        let gs = do
                seed <- arbitrary
                nbplayers <- Test.QuickCheck.elements [3 .. 7]
                return (seed, nbplayers :: Int)
        it "end well" $ forAll gs $ \(seed, nbplayers) -> case pureGame (stdGenStateStrat pure) (mkStdGen seed) (map (T.pack . show) [1 .. nbplayers]) of
                                                              (_, Right _) -> True
                                                              _ -> False

    describe "Hub" $ do
        let mhs = initialHubstate
            runHubE :: PureHub () a -> HubState -> Either PlayerError (a, HubState, [((), GameId, GameEvent)])
            runHubE a hs = runPureHub a () (mkStdGen 42) hs
            runHub :: PureHub () a -> (a, HubState, [((), GameId, GameEvent)])
            runHub a = case runHubE a mhs of
                         Left rr -> error (show rr)
                         Right x -> x
            shouldGiveError act hs err = case runHubE act hs of
                                          Left rr -> rr `shouldBe` err
                                          _ -> fail "Should have failed"
        it "Should start with an empty game" $ games mhs `shouldBe` mempty
        it "Should not enter nonexistent games" $ shouldGiveError (joinGame "bob" 12) mhs GameNotFound
        let (gid, hs1, msgs) = runHub (newGame "bob")
        it "Should create a new game" $ do
            gid `shouldBe` 0
            msgs `shouldBe` [((), 0,GameCreated)]
            games hs1 `shouldBe` M.singleton 0 (Joining (M.singleton "bob" Joined))
        let res = runHubE (joinGame "garry" 0 >> joinGame "john" 0) hs1
            Right (_, hs2, msgs2) = res
        it "Should register other players" $ do
            case res of
                Left rr -> fail (show rr)
                Right _ -> return ()
            msgs2 `shouldBe` [((), 0, PlayerJoinedGame "garry"), ((), 0, PlayerJoinedGame "john")]
            games hs2 `shouldBe` M.singleton 0 (Joining (M.fromList [("bob", Joined), ("garry", Joined), ("john", Joined)]))
        let res3 = runHubE a hs2
            a = toggleReady 0 "bob"
             >> toggleReady 0 "garry"
             >> toggleReady 0 "john"
            Right (_, hs3, msgs3) = res3
        it "Should toggle status until game starts" $ do
            case res3 of
              Left rr -> fail (show rr)
              Right _ -> return ()
            msgs3 `shouldBe` [ ((), 0, PlayerReady "bob" Ready)
                             , ((), 0, PlayerReady "garry" Ready)
                             , ((), 0, PlayerReady "john" Ready)
                             , ((), 0, GameStarted ["bob","garry","john"])
                             , ((), 0, PlayerMustPlay "bob")
                             , ((), 0, PlayerMustPlay "garry")
                             , ((), 0, PlayerMustPlay "john")
                             ]
        it "Should start the game properly" $ do
            let InGame gid' _ todo messages = playerStatus hs3 "bob"
                TodoAction age turn pid cards _ = todo
            gid' `shouldBe` 0
            todo `shouldSatisfy` has _TodoAction
            age `shouldBe` Age1
            turn `shouldBe` 1
            pid `shouldBe` "bob"
            length cards `shouldBe` 7
            messages `shouldBe` []
        let res4 = runHubE a4 hs3
            a4 = do
                InGame _ _ (TodoAction _ _ _ cardsBob _) _ <- return (playerStatus hs3 "bob")
                InGame _ _ (TodoAction _ _ _ cardsGarry _) _ <- return (playerStatus hs3 "garry")
                InGame _ _ (TodoAction _ _ _ cardsJohn _) _ <- return (playerStatus hs3 "john")
                playAction (PlayerAction Drop (head cardsBob ^. _1)) mempty Nothing 0 "bob"
                playAction (PlayerAction Drop (head cardsGarry ^. _1)) mempty Nothing 0 "garry"
                playAction (PlayerAction Drop (head cardsJohn ^. _1)) mempty Nothing 0 "john"
            Right (_, hs4, _) = res4
        it "Should be possible to play" $ do
            let InGame gid' _ todo messages = playerStatus hs4 "bob"
                TodoAction age turn pid cards _ = todo
            gid' `shouldBe` 0
            todo `shouldSatisfy` has _TodoAction
            age `shouldBe` Age1
            turn `shouldBe` 2
            pid `shouldBe` "bob"
            length cards `shouldBe` 6
            messages `shouldBe` []
