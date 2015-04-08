{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, RecordWildCards, OverloadedStrings, TemplateHaskell, TypeOperators #-}

module Main where

import           Control.Applicative
import           Control.Monad.Reader

import           Data.Aeson.TH
import           Data.Maybe

import           Servant.API
import           Servant.Server
import           Web.Users.Types

import qualified Data.Text as T

import           Command
import           Model

data Request = Request
  { rqCommand :: T.Text
  , rqOptions :: [(T.Text, T.Text)]
  } deriving (Eq, Show)

deriveJSON (opts { fieldLabelModifier     = rmvPrefix "rq"
                 , constructorTagModifier = rmvPrefix ""}) ''Request

data Response =
    Ok
  | Fail
    { rspMessage  :: T.Text
    } deriving (Eq, Show)

deriveJSON (opts { fieldLabelModifier     = rmvPrefix "rsp"
                 , constructorTagModifier = rmvPrefix ""}) ''Response

data UserData = UserData
  { usrNumber :: Maybe T.Text
  , usrSshKey :: Maybe T.Text
  }

deriveJSON (opts { fieldLabelModifier     = rmvPrefix "usr"
                 , constructorTagModifier = rmvPrefix ""}) ''UserData

type Api = "user" :> Request :> Post Response

data BE = BE

instance UserStorageBackend BE where
  type UserId BE = String
  createUser bck (User {..}) = return $ Right $ T.unpack $ u_name

data Proxy a

crUser :: UserStorageBackend bck => Command bck (IO Response)
crUser = cmd
    ( opt    "name" "User name"
    , opt    "email" "User mail"
    , opt    "password" "User password"
    , optMay "number" "User number" Nothing
    , optMay "ssh-key" "SSH public key" Nothing
    ) $ \u_name u_email password usrNumber usrSshKey bck -> do
        createUser bck (User { u_active = True
                             , u_more   = UserData { .. }
                             , u_password = makePassword $ PasswordPlain password
                             , ..
                             })
        return Ok

cmds :: UserStorageBackend bck => [(String, Command bck (IO Response))]
cmds = [ ("create-user", crUser)
       , ("delete-user", crUser)
       ]

names :: UserStorageBackend bck => bck -> Keys -> IO [Response]
names bck opts = mapM (\(name, Command {..}) -> maybe (return $ Fail "ERROR") ($ bck) (runReaderT cmdFn opts)) cmds

main :: IO ()
main = do
  return ()
