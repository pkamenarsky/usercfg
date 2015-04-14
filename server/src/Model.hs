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

data Request = Request
  { rqCommand :: T.Text
  , rqOptions :: [(T.Text, T.Text)]
  } deriving (Eq, Show)

deriveJSON' "rq" ''Request

data Response =
    Ok
  | Response Value
  | Fail Error

instance ToJSON Response where
  toJSON Ok           = A.object [ "status" .= ("ok" :: T.Text) ]
  toJSON (Response v) = A.object [ "status" .= ("ok" :: T.Text)
                                 , "response" .= v ]
  toJSON (Fail e)     = A.object [ "status" .= ("error" :: T.Text)
                                 , "error" .= toJSON e ]

data UserData = UserData
  { usrNumber  :: Maybe T.Text
  , usrSshKeys :: M.Map T.Text T.Text
  }

deriveJSON' "usr" ''UserData

data DhRequest      = DhRequest          { dhReqUser :: T.Text, dhClPub :: Integer }
data DhResponse     = DhResponse         { dhSvPub :: Integer }
data DhSignRequest  = DhSignRequest      { dhClSgnUser :: T.Text, dhClKeyHash :: T.Text, dhClSig :: T.Text }
data DhSignResponse = SignOk | SignNotOk { dhSvReason :: String }

deriveJSON' "dh" ''DhRequest
deriveJSON' "dh" ''DhResponse
deriveJSON' "dh" ''DhSignRequest
deriveJSON' "dh" ''DhSignResponse

