module Strategies.Compose where

import Startups.Interpreter
import Startups.GameTypes

import qualified Data.Map.Strict as M

-- | Given a default strategy, and a list of player-specific strategy,
-- builds a global strategy suitable for the intepreter.
composeStrat :: Strategy p m -> [(PlayerId, Strategy p m)] -> Strategy p m
composeStrat def lst = Strategy pd ac
    where
        strmap = M.fromList lst
        pd age turn pid = _doPlayerDecision (M.findWithDefault def pid strmap) age turn pid
        ac age pid = _doAskCard (M.findWithDefault def pid strmap) age pid
