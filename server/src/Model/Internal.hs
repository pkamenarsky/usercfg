module Model.Internal where

import           Data.Aeson.TH
import           Data.Char

import           Language.Haskell.TH


opts :: Options
opts = Options
  { fieldLabelModifier = id
  , constructorTagModifier = id
  , allNullaryToStringTag = True
  , omitNothingFields = True
  , sumEncoding = ObjectWithSingleField
  }

rmvPrefix :: String -> String -> String
rmvPrefix prf = hyphenize . lower . drop (length prf)
  where lower []     = []
        lower (c:cs) = toLower c:cs
        hyphenize = concatMap (\c -> if isUpper c then ['-', toLower c] else [c])

deriveJSON' :: String -> Name -> Q [Dec]
deriveJSON' prf = deriveJSON
  (opts { fieldLabelModifier     = rmvPrefix prf
        , constructorTagModifier = rmvPrefix ""})

