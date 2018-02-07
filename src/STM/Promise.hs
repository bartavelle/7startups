-- | This is a very simple promise system, for promises that can fail.
module STM.Promise ( newPromise
                   , PubFP
                   , SubFP
                   , fulfillPromise
                   , failPromise
                   , getResult
                   ) where

import Control.Concurrent.STM
import Control.Monad (void)

-- | The publising part of the promise.
newtype PubFP e a = PubFP (Either e a -> STM ())

-- | The subscribing part of the promise.
newtype SubFP e a = SubFP (STM (Either e a))

newPromise :: STM (PubFP e a, SubFP e a)
newPromise = do
    n <- newEmptyTMVar
    return (PubFP (void . tryPutTMVar n), SubFP (readTMVar n))

fulfillPromise :: PubFP e a -> a -> STM ()
fulfillPromise (PubFP o) a = o (Right a)

failPromise :: PubFP e a -> e -> STM ()
failPromise (PubFP o) e = o (Left e)

getResult :: SubFP e a -> STM (Either e a)
getResult (SubFP o) = o
