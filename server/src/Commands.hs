{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Commands where

import qualified Crypto.Hash.SHA1       as H

import qualified Data.ByteString.Base16 as B16
import qualified Data.Map               as M
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Data.Tuple.OneTuple

import qualified Network.Sendgrid.Api   as SG

import           Web.Users.Types

import           Model
import           Command

cmdResetPassword :: UserStorageBackend bck => (T.Text, Command bck (IO Response))
cmdResetPassword = cmd "reset-password" True
    (OneTuple $ opt "name" "n" "User name" None) $ \u_name bck -> do
      token <- requestPasswordReset bck u_name 1000000
      case token of
        Just token' -> do
          SG.sendEmail ( SG.Authentication "USERNAME" "PASSWORD" )
                       $ SG.EmailMessage { to      = "owain@owainlewis.com"
                                         , from    = ""
                                         , subject = "Hello World"
                                         , text    = T.unpack $ unPasswordResetToken token'
                                         }
          return responseOk
        Nothing -> return responseOk

cmdCreateUser :: UserStorageBackend bck => (T.Text, Command bck (IO Response))
cmdCreateUser = cmd "create-user" False
    ( opt    "name" "n" "User name" None
    , opt    "email" "e" "User mail" None
    , opt    "password" "p" "User password" None
    , optMay "number" "N" "User number" None Nothing
    , optMay "ssh-key" "S" "SSH public key" None Nothing
    ) $ \u_name u_email password usrNumber sshKey bck -> do
        let sshKeyHash = maybe "" (TE.decodeUtf8 . B16.encode . H.hash . TE.encodeUtf8) sshKey

        either (return . responseFail . UserStorageBackendError)
               (const $ return responseOk) =<<
          createUser bck (User
            { u_active = True
            , u_more   = UserData
                { usrSshKeys = maybe M.empty (M.singleton sshKeyHash) sshKey
                , ..
                }
            , u_password = makePassword $ PasswordPlain password
            , ..
            })

commands :: UserStorageBackend bck => [(T.Text, Command bck (IO Response))]
commands =
  [ cmdCreateUser
  , cmdAuth "delete-user" True noArgs $ \_ uid bck -> deleteUser bck uid >> return responseOk
  ]
