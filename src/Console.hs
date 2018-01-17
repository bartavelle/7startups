{-# LANGUAGE OverloadedStrings #-}
module Main where

import Startups.Interpreter
import Startups.Utils
import Startups.Game
import Startups.GameTypes
import Backends.Common
import Strategies.Random
import Strategies.Compose

import Control.Lens
import System.Random
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Data.Char (isDigit)
import qualified Data.List.NonEmpty as NE

readNumber :: IO Int
readNumber = do
    n <- getLine
    return $ if all isDigit n
        then read n
        else -1

randStrat :: Strategy Identity IO
randStrat = randStrategy (curry randomRIO)

playerStrat :: Strategy Identity IO
playerStrat = Strategy pd ac
    where
        pd age turn pid necards stt = do
            let pm = stt ^. playermap
                x = allowableActions age pid necards pm
            print (PP.pretty (quicksituation age turn pm))
            print (PP.pretty (playerActionsDialog pid pm necards x))
            r <- readNumber
            if r >= 0 && r < NE.length x
                then return (return (x NE.!! r))
                else pd age turn pid necards stt
        ac turn pid necards stt m = do
            let cards = _NonEmpty # necards
                pm = stt ^. playermap
            print (PP.pretty m)
            print (PP.pretty (cardChoiceDialog pid pm cards))
            n <- readNumber
            if n >= 0 && n < length cards
                then return (return (cards !! n))
                else ac turn pid necards stt m

consoleDict :: OperationDict Identity IO
consoleDict = OperationDict (composeStrat randStrat [("you", playerStrat)]) (return . Right . runIdentity) msg
    where
        msg gs (PlayerCom "you" m) = com gs m
        msg gs (BroadcastCom m)    = com gs m
        msg _ _ = return ()
        com _ (RawMessage m) = print (PP.pretty m)
        com stt (ActionRecapMsg _ _ _ actions) = print (PP.pretty (displayActions (stt ^. playermap) actions))

main :: IO ()
main = do
    g <- newStdGen
    (_, o) <- runInterpreter consoleDict (initialGameState g ["you","pim","pam"]) playGame
    case o of
        Left rr -> error (show (PP.pretty rr))
        Right x -> print (PP.pretty (displayVictory x))
