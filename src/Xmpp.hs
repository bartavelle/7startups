module Main where

import Xmpp.Backend
import Backends.Hub

import qualified Data.Text as T
import System.Environment

main :: IO ()
main = do
    (domain : servername : username : password : confroom : []) <- getArgs
    xmpp <- runXmpp domain servername 5222 (T.pack username) (T.pack password) (T.pack confroom)
    runHub [xmpp]
