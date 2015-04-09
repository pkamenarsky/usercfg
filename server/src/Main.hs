{-# LANGUAGE DataKinds, ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, RecordWildCards, OverloadedStrings, TemplateHaskell, TypeOperators #-}

module Main where

import           Control.Applicative
import           Control.Monad.Reader

import           Data.Aeson
import qualified Data.Aeson             as A
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

data Error = forall e. ToJSON e => UserStorageBackendError e
           | InvalidUserError
           | NoSuchCommandError

instance ToJSON CreateUserError where
  toJSON _ = A.String "create_user"

instance ToJSON UpdateUserError where
  toJSON _ = A.String "update_user"

instance ToJSON TokenError where
  toJSON _ = A.String "token"

instance ToJSON Error where
  toJSON (UserStorageBackendError e) = toJSON e
  toJSON InvalidUserError            = A.String "invalid_user"
  toJSON NoSuchCommandError          = A.String "no_such_command"

data Request = Request
  { rqCommand :: T.Text
  , rqOptions :: [(T.Text, T.Text)]
  } deriving (Eq, Show)

deriveJSON (opts { fieldLabelModifier     = rmvPrefix "rq"
                 , constructorTagModifier = rmvPrefix ""}) ''Request

data Response =
    Ok
  | Response Value
  | Fail Error

instance ToJSON Response where
  toJSON Ok           = object [ "status" .= ("ok" :: T.Text) ]
  toJSON (Response v) = object [ "status" .= ("ok" :: T.Text)
                               , "response" .= v ]
  toJSON (Fail e)     = object [ "status" .= ("error" :: T.Text)
                               , "error" .= toJSON e ]

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

cmds :: UserStorageBackend bck => Proxy bck -> [Command bck (IO Response)]
cmds _ =
  [ cmd "create-user" False
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
  , cmd "delete-user" True
    ( opt    "name" "User name"
    , opt    "password" "User password"
    ) $ \name password bck -> do
        sId <- authUser bck name (PasswordPlain password) 0
        case sId of
          Just sId' -> do
            userId <- verifySession bck sId' 0
            case userId of
              Just userId' -> do
                deleteUser bck userId'
                return Ok
              Nothing -> return $ Fail InvalidUserError
          Nothing -> return $ Fail InvalidUserError
  ]

mkProxy :: a -> Proxy a
mkProxy _ = Proxy

names :: UserStorageBackend bck => bck -> Keys -> IO [Response]
names bck opts = mapM (\Command {..} -> maybe (return $ Fail NoSuchCommandError) ($ bck) (runReaderT cmdFn opts)) (cmds $ mkProxy bck)

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
