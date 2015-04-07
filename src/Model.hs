module Model where

import Data.Aeson.TH
import Data.Char

rmvPrefix :: String -> String -> String
rmvPrefix prf = lower . drop (length prf)
  where lower []     = []
        lower (c:cs) = toLower c:cs

opts :: Options
opts = Options
  { fieldLabelModifier = id
  , constructorTagModifier = id
  , allNullaryToStringTag = True
  , omitNothingFields = True
  , sumEncoding = ObjectWithSingleField
  }
