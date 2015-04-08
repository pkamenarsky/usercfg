{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, RecordWildCards, OverloadedStrings, TemplateHaskell, TypeOperators #-}

module Main where

import           Control.Applicative

import           Data.Aeson.TH

import           Servant.API
import           Servant.Server
import           Web.Users.Types

import qualified Data.Text as T

import           Command
import           Model

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

cmdCreateUser = Command "create-user"
  ( Option "name" "User name" ()            :: Opt T.Text
  , Option "email" "User mail" ()           :: Opt T.Text
  , Option "password" "User password" ()    :: Opt T.Text
  , Option "number" "User number" Nothing   :: OptMay T.Text
  , Option "ssh-key" "User ssh key" Nothing :: OptMay T.Text
  ) $ \b u_name u_email password usrNumber usrSshKey -> do
      createUser b (User { u_active = True
                         , u_more   = UserData { .. }
                         , u_password = makePassword $ PasswordPlain password
                         , ..
                         })
      return Ok

main :: IO ()
main = do
  return ()
