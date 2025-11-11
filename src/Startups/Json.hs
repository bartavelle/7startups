module Startups.Json where

import Data.Aeson.TH
  ( Options
      ( allNullaryToStringTag,
        constructorTagModifier,
        fieldLabelModifier,
        sumEncoding
      ),
    SumEncoding (ObjectWithSingleField),
    defaultOptions,
  )
import Data.Char (toLower)

baseOptions :: Options
baseOptions = defaultOptions {allNullaryToStringTag = True, sumEncoding = ObjectWithSingleField}

dropOptions :: Int -> Options
dropOptions n = baseOptions {fieldLabelModifier = map toLower . drop n, constructorTagModifier = drop n}
