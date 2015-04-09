{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, RecordWildCards, OverloadedStrings, TemplateHaskell, TypeOperators #-}

module Main where

import           Control.Applicative
import           Control.Monad.Reader

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Maybe
import           Data.Proxy

import           Servant.API
import           Servant.Server
import           Web.Users.Types

import qualified Data.Text              as T

import           Network.Wai.Handler.Warp

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
  | Response Value
  | Fail T.Text
    deriving (Eq, Show)

deriveJSON (opts { fieldLabelModifier     = rmvPrefix "rsp"
                 , constructorTagModifier = rmvPrefix ""}) ''Response

data UserData = UserData
  { usrNumber :: Maybe T.Text
  , usrSshKey :: Maybe T.Text
  }

deriveJSON (opts { fieldLabelModifier     = rmvPrefix "usr"
                 , constructorTagModifier = rmvPrefix ""}) ''UserData

type Api = "info" :> Get Response

api :: Proxy Api
api = Proxy

data BE = BE

instance UserStorageBackend BE where
  type UserId BE = String
  createUser bck (User {..}) = return $ Right $ T.unpack $ u_name

crUser :: UserStorageBackend bck => Command bck (IO Response)
crUser = cmd "create-user"
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

cmds :: UserStorageBackend bck => Proxy bck -> [Command bck (IO Response)]
cmds _ = [ crUser
         , crUser
         ]

mkProxy :: a -> Proxy a
mkProxy _ = Proxy

names :: UserStorageBackend bck => bck -> Keys -> IO [Response]
names bck opts = mapM (\Command {..} -> maybe (return $ Fail "ERROR") ($ bck) (runReaderT cmdFn opts)) (cmds $ mkProxy bck)

main :: IO ()
main = do
  return ()

server :: Server Api
server = info
  where
    info = do
      return $ Response $ toJSON $ cmds (Proxy :: Proxy BE)

runServer :: IO ()
runServer = do
  run 8000 $ serve api $ server
