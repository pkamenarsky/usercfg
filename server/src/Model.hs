{-# LANGUAGE ExistentialQuantification, OverloadedStrings, TemplateHaskell #-}

module Model where

import           Data.Aeson
import qualified Data.Aeson             as A
import qualified Data.Map               as M
import           Data.Proxy
import qualified Data.Text              as T

import           Web.Users.Types

import           Model.Internal

mkProxy :: a -> Proxy a
mkProxy _ = Proxy

data Error = forall e. ToJSON e => UserStorageBackendError e
           | InvalidUserError
           | NoSuchCommandError
           | SignAlgoNotSshRsa
           | SignVerify

instance ToJSON CreateUserError where
  toJSON UsernameOrEmailAlreadyTaken = A.String "create_user_user_or_email_taken"
  toJSON InvalidPassword             = A.String "create_user_invalid_password"

instance ToJSON UpdateUserError where
  toJSON UsernameOrEmailAlreadyExists = A.String "update_user_user_or_email_exists"
  toJSON UserDoesntExit               = A.String "update_user_user_doesnt_exist"

instance ToJSON TokenError where
  toJSON _ = A.String "token"

instance ToJSON Error where
  toJSON (UserStorageBackendError e) = toJSON e
  toJSON InvalidUserError            = A.String "invalid_user"
  toJSON NoSuchCommandError          = A.String "no_such_command"
  toJSON SignAlgoNotSshRsa           = A.String "algo-not-ssh-rsa"
  toJSON SignVerify                  = A.String "verify"

data UserData = UserData
  { usrNumber  :: Maybe T.Text
  , usrSshKeys :: M.Map T.Text T.Text
  }

deriveJSON' "usr" ''UserData

data DhRequest    = DhRequest    { dhReqUser   :: T.Text, dhClPub :: Integer } deriving Show
data DhCmdRequest = DhCmdRequest { dhClSgnUser :: T.Text
                                 , dhClCommand :: T.Text
                                 , dhClOptions :: [(T.Text, T.Text)]
                                 , dhClPass    :: Maybe T.Text
                                 , dhClSig     :: Maybe (T.Text, T.Text)
                                 } deriving Show

deriveJSON' "dh" ''DhRequest
deriveJSON' "dh" ''DhCmdRequest

data Response = Ok | Response Value | Fail Error

instance ToJSON Response where
  toJSON Ok           = A.object [ "status" .= ("ok" :: T.Text) ]
  toJSON (Response v) = A.object [ "status" .= ("ok" :: T.Text)
                                 , "response" .= v ]
  toJSON (Fail e)     = A.object [ "status" .= ("error" :: T.Text)
                                 , "error" .= toJSON e ]

