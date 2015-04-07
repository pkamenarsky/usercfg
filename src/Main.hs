{-# LANGUAGE DataKinds, RecordWildCards, OverloadedStrings, TemplateHaskell, TypeOperators #-}

module Main where

import           Control.Applicative
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

data UserData = UserData
  { usrNumber  :: Maybe T.Text
  }

deriveJSON (opts { fieldLabelModifier     = rmvPrefix "usr"
                 , constructorTagModifier = rmvPrefix ""}) ''UserData

type Api = "user" :> Request :> Post Response

parse :: UserStorageBackend b => b -> Request -> IO Response
parse b (Request {..}) = undefined
  where
    lkp opt = lookup opt rqOptions

    cmd "--create-user" = do
      u_name     <- lkp "name"
      u_email    <- lkp "email"
      u_password <- makePassword . PasswordPlain <$> lkp "password"
      usrNumber  <- pure <$> lkp "number"

      return $ createUser b (User { u_active = True
                                  , u_more   = UserData { .. }
                                  , ..
                                  })


main :: IO ()
main = do
  return ()
