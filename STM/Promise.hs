-- | This is a very simple promise system, for promises that can fail.
module STM.Promise ( newFPromise
                   , PubFP
                   , SubFP
                   , fulfillFP
                   , failFP
                   , getResult
                   , addPubAction
                   ) where

import Control.Concurrent.STM
import Data.Monoid

-- | The publising part of the promise. Its monoid instance writes to all
-- publishers.
newtype PubFP e a = PubFP (Either e a -> STM ())

-- | The subscribing part of the promise. Its monoid instance returns the
-- first result of the compounded subscribers.
newtype SubFP e a = SubFP (STM (Either e a))

newFPromise :: STM (PubFP e a, SubFP e a)
newFPromise = do
    n <- newEmptyTMVar
    return (PubFP (putTMVar n), SubFP (readTMVar n))

instance Monoid (SubFP e a) where
    mempty = SubFP retry
    SubFP a `mappend` SubFP b = SubFP (a `orElse` b)

instance Monoid (PubFP e a) where
    mempty = PubFP (const (return ()))
    PubFP a `mappend` PubFP b = PubFP (a >> b)

fulfillFP :: PubFP e a -> a -> STM ()
fulfillFP (PubFP o) a = o (Right a)

failFP :: PubFP e a -> e -> STM ()
failFP (PubFP o) e = o (Left e)

getResult :: SubFP e a -> STM (Either e a)
getResult (SubFP o) = o

addPubAction :: STM () -> PubFP e a -> PubFP e a
addPubAction a (PubFP o) = PubFP (\x -> a >> o x)
