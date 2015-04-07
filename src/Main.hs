{-# LANGUAGE DataKinds, OverloadedStrings, TemplateHaskell, TypeOperators #-}

module Main where

import           Data.Aeson.TH

import           Servant.API
import           Servant.Server
import           Web.Users.Types

import qualified Data.Text as T

import           Model

data Request = Request
  { rqCommand :: T.Text
  , rqOptions :: [(T.Text, T.Text)]
  }

deriveJSON (opts { fieldLabelModifier     = rmvPrefix "rq"
                 , constructorTagModifier = rmvPrefix ""}) ''Request

data Response =
    Ok
  | Fail
    { rspMessage  :: T.Text
    }

deriveJSON (opts { fieldLabelModifier     = rmvPrefix "rsp"
                 , constructorTagModifier = rmvPrefix ""}) ''Response

f c = do
  a <- authUser c "asd" "asd" undefined
  return a

type Api = "push" :> Request :> Post Response

main :: IO ()
main = do
  return ()
