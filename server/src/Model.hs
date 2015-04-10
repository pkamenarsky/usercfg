module Model where

import Data.Aeson.TH
import Data.Char

deriveJSON' prf = deriveJSON
  (opts { fieldLabelModifier     = rmvPrefix prf
        , constructorTagModifier = rmvPrefix ""})

rmvPrefix :: String -> String -> String
rmvPrefix prf = hyphenize . lower . drop (length prf)
  where lower []     = []
        lower (c:cs) = toLower c:cs
        hyphenize = concatMap (\c -> if isUpper c then ['-', toLower c] else [c])

opts :: Options
opts = Options
  { fieldLabelModifier = id
  , constructorTagModifier = id
  , allNullaryToStringTag = True
  , omitNothingFields = True
  , sumEncoding = ObjectWithSingleField
  }
