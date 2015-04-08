{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, OverloadedStrings, TemplateHaskell, TypeOperators #-}

module Main where

import           Control.Applicative

import           Data.Aeson.TH

import           Servant.API
import           Servant.Server
import           Web.Users.Types

import qualified Data.Text as T

import           Command
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
  { usrNumber :: Maybe T.Text
  , usrSshKey :: Maybe T.Text
  }

deriveJSON (opts { fieldLabelModifier     = rmvPrefix "usr"
                 , constructorTagModifier = rmvPrefix ""}) ''UserData

type Api = "user" :> Request :> Post Response

cmdCreateUser = Command "create-user"
  ( Option "name" "User name" ()            :: Opt T.Text
  , Option "email" "User mail" ()           :: Opt T.Text
  , Option "password" "User password" ()    :: Opt T.Text
  , Option "number" "User number" Nothing   :: OptMay T.Text
  , Option "ssh-key" "User ssh key" Nothing :: OptMay T.Text
  )
  cmdCreateUserFn
    where

      -- cmdCreateUserFn :: UserStorageBackend bck => bck -> T.Text -> T.Text -> T.Text -> Maybe T.Text -> Maybe T.Text -> IO (Either CreateUserError (UserId bck))
      cmdCreateUserFn b u_name u_email password usrNumber usrSshKey = do
        createUser b (User { u_active = True
                           , u_more   = UserData { .. }
                           , u_password = makePassword $ PasswordPlain password
                           , ..
                           })
        return ()

{-
parse :: UserStorageBackend b => b -> Request -> IO Response
parse b (Request {..}) = undefined
  where
    lkp opt = lookup opt rqOptions

    cmd "--create-user" = do
      u_name     <- lkp "name"
      u_email    <- lkp "email"
      u_password <- makePassword . PasswordPlain <$> lkp "password"
      usrNumber  <- pure <$> lkp "number"
      usrSshKey  <- pure <$> lkp "ssh-key"

      return $ createUser b (User { u_active = True
                                  , u_more   = UserData { .. }
                                  , ..
                                  })
-}

main :: IO ()
main = do
  return ()
