{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, OverloadedStrings, TemplateHaskell, TypeOperators #-}

module Main where

import           Control.Applicative

import           Data.Aeson.TH
import           Data.Maybe

import           Servant.API
import           Servant.Server
import           Web.Users.Types

import qualified Data.Text as T

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

data Option a = Option
  { optName     :: T.Text
  , optDesc     :: T.Text
  , optOptional :: Bool
  , optDefault  :: Maybe a
  }

data Command opt optLk = Command
  { cmdName     :: T.Text
  , cmdOptions  :: opt
  , cmdFn       :: optLk -> IO ()
  }

cmdCreateUser = Command "create-user"
  ( Option "name" "User name" False (Nothing :: Maybe T.Text)
  , Option "email" "User mail" False (Nothing :: Maybe T.Text)
  , Option "password" "User password" False (Nothing :: Maybe T.Text)
  , Option "number" "User number" True (Nothing :: Maybe T.Text)
  , Option "ssh-key" "User ssh key" True (Nothing :: Maybe T.Text)
  )
  cmdCreateUserFn

cmdCreateUserFn :: (T.Text, T.Text, T.Text, T.Text, T.Text) -> IO ()
cmdCreateUserFn (name, email, password, number, sshKey) = do
  print name
  return ()

class Exec opt optLk where
  exec :: [(T.Text, T.Text)] -> Command opt optLk -> IO ()

instance (Read a, Read b, Read c, Read d, Read e) =>
         Exec (Option a, Option b, Option c, Option d, Option e)
         (a, b, c, d, e) where
  exec opts (Command {..}) = fromMaybe (return ()) $ do
    let (a, b, c, d, e) = cmdOptions

    a' <- (read . T.unpack) <$> lookup (optName a) opts
    b' <- (read . T.unpack) <$> lookup (optName b) opts
    c' <- (read . T.unpack) <$> lookup (optName c) opts
    d' <- (read . T.unpack) <$> lookup (optName d) opts
    e' <- (read . T.unpack) <$> lookup (optName e) opts

    return $ cmdFn (a', b', c', d', e')

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


main :: IO ()
main = do
  return ()
