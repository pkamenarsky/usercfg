{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Commands where

import           Control.Applicative
import           Control.Monad.Trans.Maybe
import           Control.Monad.IO.Class

import qualified Crypto.Hash.SHA1           as H

import qualified Data.ByteString.Base16     as B16
import           Data.List                  (intercalate)
import           Data.List.Split            (splitOn)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Tuple.OneTuple

import qualified Network.Sendgrid.Api       as SG

import           System.Environment

import           Web.Users.Types

import           Model
import           Command

cmdResetPassword :: UserStorageBackend bck => (T.Text, Command bck (IO Response))
cmdResetPassword = cmd "reset-password" True
    (OneTuple $ opt "name" "n" "User name" None) $ \username bck -> do
      runMaybeT $ do
        userId <- MaybeT $ getUserIdByName bck username
        user   <- MaybeT $ getUserById bck userId :: MaybeT IO (User UserData)
        token  <- MaybeT $ Just <$> requestPasswordReset bck userId 1000000

        sgUser <- MaybeT $ lookupEnv "SG_USER"
        sgPass <- MaybeT $ lookupEnv "SG_PASS"
        sgFrom <- MaybeT $ lookupEnv "SG_FROM"
        sgSubj <- MaybeT $ lookupEnv "SG_SUBJ"
        sgText <- MaybeT $ lookupEnv "SG_TEXT"

        liftIO $ SG.sendEmail (SG.Authentication sgUser sgPass)
          $ SG.EmailMessage
            { to      = T.unpack $ u_email user
            , from    = sgFrom
            , subject = sgSubj
            , text    = intercalate ( T.unpack $ unPasswordResetToken token )
                                    $ splitOn "$TOKEN" sgText
            }

      return responseOk

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
  , cmdResetPassword
  , cmdAuth "delete-user" True noArgs $ \_ uid bck -> deleteUser bck uid >> return responseOk
  ]
