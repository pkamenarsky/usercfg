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

crUser :: UserStorageBackend bck => Resolve (bck -> IO Response)
crUser = apply
    ( opt $ Option "name" "User name" ()
    , opt $ Option "email" "User mail" ()
    , opt $ Option "password" "User password" ()
    ) $ \u_name u_email password bck -> do
        createUser bck (User { u_active = True
                             , u_more   = UserData {}
                             , u_password = makePassword $ PasswordPlain password
                             , ..
                             })
        return Ok

cmds :: UserStorageBackend bck => Proxy bck -> [(String, Resolve (bck -> IO Response))]
cmds _ = [ ("create-user", crUser)
         , ("delete-user", crUser)
         ]

names :: UserStorageBackend bck => bck -> Keys -> IO [Response]
names bck opts = mapM (\(name, cmd) -> maybe (return $ Fail "ERROR") ($ bck) (runReaderT cmd opts)) (cmds (undefined :: Proxy bck))

main :: IO ()
main = do
  return ()
