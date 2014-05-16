{-# LANGUAGE OverloadedStrings #-}
module Main where

import Startups.Interpreter
import Startups.Utils
import Startups.Game
import Startups.GameTypes
import Backends.Common

import Control.Lens
import Control.Applicative
import System.Random
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Data.Char (isDigit)

readNumber :: IO Int
readNumber = do
    n <- getLine
    return $ if all isDigit n
        then read n
        else -1

consoleDict :: OperationDict Identity IO
consoleDict = OperationDict (Strategy pd ac) (return . Right . runIdentity) msg
    where
        pd age turn pid necards stt = do
            let cards = _NonEmpty # necards
                pm = stt ^. playermap
                x = allowableActions age pid cards pm
            r <- case pid of
                    "you" -> do
                        print (PP.pretty (quicksituation age turn pm))
                        print (PP.pretty (playerActionsDialog pid pm cards x))
                        readNumber
                    _ -> randomRIO (0, length x - 1)
            if r >= 0 && r < length x
                then let (pa,e,_) = x !! r
                     in  return (return (pa, e))
                else pd age turn pid necards stt
        ac turn pid necards stt m = do
            let cards = _NonEmpty # necards
                pm = stt ^. playermap
            case pid of
                "you" -> do
                    print (PP.pretty m)
                    print (PP.pretty (cardChoiceDialog pid pm cards))
                    n <- readNumber
                    if n >= 0 && n < length cards
                        then return (cards !! n)
                        else ac turn pid necards stt m
                _ -> (cards !!) <$> randomRIO (0, length cards - 1)
        msg gs (PlayerCom "you" m) = com gs m
        msg gs (BroadcastCom m)    = com gs m
        msg _ _ = return ()
        com _ (RawMessage m) = print (PP.pretty m)
        com stt (ActionRecapMsg actions) = print (PP.pretty (displayActions (stt ^. playermap) actions))

main :: IO ()
main = do
    g <- newStdGen
    (_, o) <- runInterpreter consoleDict (initialGameState g ["you","pim","pam"]) playGame
    case o of
        Left rr -> error (show (PP.pretty rr))
        Right x -> print (PP.pretty (displayVictory x))
