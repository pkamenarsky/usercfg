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

import           Web.Stripe
import           Web.Stripe.Plan

import           Command
import           Crypto
import           Model

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

deriveJSON (opts { fieldLabelModifier     = rmvPrefix "rq"
                 , constructorTagModifier = rmvPrefix ""}) ''Request

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

class PaymentStorageBackend a

cmdsall :: (UserStorageBackend bck, PaymentStorageBackend bck) => Proxy bck -> [Command bck (IO Response)]
cmdsall bck = cmds bck ++ cmdsp bck

cmdsp :: PaymentStorageBackend bck => Proxy bck -> [Command bck (IO Response)]
cmdsp = undefined

cmds :: UserStorageBackend bck => Proxy bck -> [Command bck (IO Response)]
cmds _ =
  [ cmd "create-user" False
    ( opt    "name" "User name"
    , opt    "email" "User mail"
    , opt    "password" "User password"
    , optMay "number" "User number" Nothing
    , optMay "ssh-key" "SSH public key" Nothing
    ) $ \u_name u_email password usrNumber usrSshKey bck -> do
        either (return . Fail . UserStorageBackendError) (const $ return Ok) =<<
          createUser bck (User { u_active = True
                               , u_more   = UserData { .. }
                               , u_password = makePassword $ PasswordPlain password
                               , ..
                               })
  , cmd "delete-user" True
    ( opt "name" "User name"
    , opt "password" "User password"
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

{-
unsafeAddPayment :: UserStorageBackend bck => Command bck (NoAuthT IO Response)
unsafeAddPayment =  cmd "add-payment" True
    ( opt "name" "User name"
    -- , opt "password" "User password"
    , opt "plan" "Payment plan"
    ) $ \name {- password -} (plan :: T.Text) bck -> do
        sId <- unsafeAuthUser bck name 0
        return Ok
-}

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
