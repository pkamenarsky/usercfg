{-# LANGUAGE CPP, ExistentialQuantification, FlexibleInstances, OverloadedStrings, RecordWildCards, TemplateHaskell, TypeSynonymInstances #-}

module Model where

import           Data.Aeson
import qualified Data.Aeson             as A
import qualified Data.ByteString        as B
import           Data.List
import           Data.Monoid
import qualified Data.Map               as M
import           Data.Proxy
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Vector            as V

import           Web.Users.Types

import           Model.Internal

mkProxy :: a -> Proxy a
mkProxy _ = Proxy

object' :: [(T.Text, Value)] -> Value
object' = object . filter (not . isDflt . snd)
  where
    isDflt Null       = True
    isDflt (Bool b)   = not b
    isDflt (Array a)  = V.null a
    isDflt (String a) = T.null a
    isDflt _          = False

type Keys = [(T.Text, T.Text)]

data Error = forall e. ToJSON e => UserStorageBackendError e
           | InvalidUserError
           | NoSuchCommandError
           | ParseError String
           | NoSharedKeyError
           | PubKeyFormatError
           | NoPubKeyError
           | MissingOptionsError
           | SignVerifyError
           | AuthError
           | AuthKeyError
           | AuthPassError
           | AuthNeededError

instance ToJSON CreateUserError where
  toJSON UsernameOrEmailAlreadyTaken = A.String "create_user_user_or_email_taken"
  toJSON InvalidPassword             = A.String "create_user_invalid_password"

instance ToJSON UpdateUserError where
  toJSON UsernameOrEmailAlreadyExists = A.String "update_user_user_or_email_exists"
  toJSON UserDoesntExit               = A.String "update_user_user_doesnt_exist"

instance ToJSON TokenError where
  toJSON _ = A.String "token"

instance ToJSON Error where
  toJSON (UserStorageBackendError e) = object [ "code" .= toJSON e ]
  toJSON NoSuchCommandError          = object [ "code" .= A.String "no_such_command" ]
  toJSON MissingOptionsError         = object [ "code" .= A.String "missing_options" ]
#ifdef DEBUG
  toJSON InvalidUserError            = object [ "code" .= A.String "invalid_user" ]
  toJSON (ParseError e)              = object [ "code" .= A.String "parse_error", "reason" .= A.String (T.pack e) ]
  toJSON NoSharedKeyError            = object [ "code" .= A.String "no_shared_key" ]
  toJSON PubKeyFormatError           = object [ "code" .= A.String "pubkey_format_error" ]
  toJSON NoPubKeyError               = object [ "code" .= A.String "no_pubkey" ]
  toJSON SignVerifyError             = object [ "code" .= A.String "verify" ]
  toJSON AuthError                   = object [ "code" .= A.String "auth" ]
  toJSON AuthKeyError                = object [ "code" .= A.String "auth_key" ]
  toJSON AuthPassError               = object [ "code" .= A.String "auth_pass" ]
  toJSON AuthNeededError             = object [ "code" .= A.String "auth_needed" ]
#else
  toJSON InvalidUserError            = object [ "code" .= A.String "auth" ]
  toJSON (ParseError _)              = object [ "code" .= A.String "auth" ]
  toJSON NoSharedKeyError            = object [ "code" .= A.String "auth" ]
  toJSON PubKeyFormatError           = object [ "code" .= A.String "auth" ]
  toJSON NoPubKeyError               = object [ "code" .= A.String "auth" ]
  toJSON SignVerifyError             = object [ "code" .= A.String "auth" ]
  toJSON AuthError                   = object [ "code" .= A.String "auth" ]
  toJSON AuthKeyError                = object [ "code" .= A.String "auth" ]
  toJSON AuthPassError               = object [ "code" .= A.String "auth" ]
  toJSON AuthNeededError             = object [ "code" .= A.String "auth" ]
#endif

data UserData = UserData
  { usrNumber  :: Maybe T.Text
  , usrSshKeys :: M.Map T.Text T.Text
  }

deriveJSON' "usr" ''UserData

data DhRequest    = DhRequest    { dhReqHash   :: T.Text }
data DhCmdRequest = DhCmdRequest { dhClCommand :: T.Text
                                 , dhClOptions :: Keys
                                 , dhClSig     :: Maybe (T.Text, T.Text)
                                 } deriving Show

hashCmdRequest :: DhCmdRequest -> B.ByteString
hashCmdRequest DhCmdRequest {..} =
  TE.encodeUtf8 $ dhClCommand
               <> (T.concat $ sort $ map (uncurry T.append) dhClOptions)

deriveJSON' "dhReq" ''DhRequest
deriveJSON' "dhCl" ''DhCmdRequest

type Response = Either Error Value

response :: Value -> Response
response = Right

responseOk :: Response
responseOk = Right ""

responseFail :: Error -> Response
responseFail = Left

instance ToJSON Response where
  toJSON (Right v) = object' [ "status" .= ("ok" :: T.Text)
                             , "response" .= v ]
  toJSON (Left e)  = object' [ "status" .= ("error" :: T.Text)
                             , "error" .= toJSON e ]

