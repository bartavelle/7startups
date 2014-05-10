{-# LANGUAGE OverloadedStrings #-}
module Main where

import Startups.Base
import Startups.Cards
import Startups.CardList
import Startups.GameTypes
import Startups.Utils

import Control.Lens
import Data.List (foldl')
import Test.Hspec
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import System.Random
import Data.Monoid
import Control.Monad
import Data.Maybe (fromJust)

getCard :: T.Text -> Card
getCard n = case filter (\c -> c ^? cName == Just n) allcards of
                (c:_) -> c
                [] -> error (T.unpack n <> " could not be found")

-- | Some game state that is good enough for testing things
testState :: GameState
testState = GameState (M.fromList players) discard (mkStdGen 5)
    where
        players = [ ("pim", pim), ("pam", pam), ("poum", poum), ("bob", bob )]
        ppim = CompanyProfile Facebook A
        ppam = CompanyProfile Apple B
        ppoum = CompanyProfile Google A
        pbob = CompanyProfile Twitter A
        pim  = PlayerState ppim Project pimcards 1 (M.fromList [(NLeft, "pam"), (NRight, "bob")]) []
        pam  = PlayerState ppam Project pamcards 3 (M.fromList [(NLeft, "poum"), (NRight, "pim")]) []
        poum = PlayerState ppoum Project poumcards 6 (M.fromList [(NLeft, "bob"), (NRight, "pam")]) []
        bob  = PlayerState pbob Project bobcards 5 (M.fromList [(NLeft, "pim"), (NRight, "poum")]) []
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
        discard = []

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
