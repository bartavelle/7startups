{-# LANGUAGE OverloadedStrings #-}
module Main where

import Startups.Base
import Startups.Cards
import Startups.CardList

import Control.Lens
import Data.List (foldl')
import Test.Hspec
import qualified Data.Set as S

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
