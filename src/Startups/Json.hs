module Startups.Json where

import Data.Aeson.TH
import Data.Char

baseOptions :: Options
baseOptions = defaultOptions { allNullaryToStringTag = True, sumEncoding = ObjectWithSingleField }

dropOptions :: Int -> Options
dropOptions n = baseOptions { fieldLabelModifier = map toLower . drop n, constructorTagModifier = drop n }
