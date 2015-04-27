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
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Tuple.OneTuple

import qualified Network.Sendgrid.Api       as SG

import           System.Environment

import           Web.Users.Types

import           Model
import           Command

cmdResetPassword :: UserStorageBackend bck => (T.Text, Command bck (IO Response))
cmdResetPassword = cmd "reset-password" "Send email with reset token" False
  (OneTuple userOption) $ \username bck -> do
    runMaybeT $ do
      userId <- MaybeT $ getUserIdByName bck username
      user   <- MaybeT $ getUserById bck userId :: MaybeT IO (User UserData)
      token  <- MaybeT $ Just <$> requestPasswordReset bck userId 1000000

      liftIO $ print token

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

cmdApplyPassword :: UserStorageBackend bck => (T.Text, Command bck (IO Response))
cmdApplyPassword = cmd "apply-password" "Set password using token requested by reset-password" False
  ( passOption
  , opt "token" "t" "Password reset token obtained by reset-password" None
  ) $ \pass token bck -> do
      either (return . responseFail . UserStorageBackendError)
             (const $ return responseOk) =<<
               applyNewPassword bck (PasswordResetToken token) (makePassword $ PasswordPlain pass)

cmdCreateUser :: UserStorageBackend bck => (T.Text, Command bck (IO Response))
cmdCreateUser = cmd "create-user" "Create new user" False
  ( userOption
  , opt    "email" "e" "User mail" None
  , opt    "password" "p" "User password" InvisibleRepeat -- make optional, generate random password
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

cmdUpdateUser :: UserStorageBackend bck => (T.Text, Command bck (IO Response))
cmdUpdateUser = cmdAuth "update-user" "Update existing user" False
  ( optMay "email" "e" "User mail" None Nothing
  , optMay "number" "N" "User number" None Nothing
  , optMay "ssh-key" "S" "SSH public key" None Nothing
  ) $ \email newUsrNumber sshKey uid bck -> do
      let sshKeyHash = maybe "" (TE.decodeUtf8 . B16.encode . H.hash . TE.encodeUtf8) sshKey

      either (return . responseFail . UserStorageBackendError)
             (const $ return responseOk) =<<
               updateUser bck uid (\old -> old
                 { u_more = (u_more old)
                     { usrSshKeys = maybe (usrSshKeys $ u_more old) (flip (M.insert sshKeyHash) (usrSshKeys $ u_more old)) sshKey
                     , usrNumber  = fromMaybe (usrNumber $ u_more old) newUsrNumber
                     }
                 , u_email = fromMaybe (u_email old) email
                 })

cmdDeleteUser :: UserStorageBackend bck => (T.Text, Command bck (IO Response))
cmdDeleteUser =
  cmdAuth "delete-user" "Delete existing user" True noArgs $ \_ uid bck -> deleteUser bck uid >> return responseOk

cmdUpdatePassword :: UserStorageBackend bck => (T.Text, Command bck (IO Response))
cmdUpdatePassword = cmdAuth "update-password" "Update password of existing user" False
  ( OneTuple $ opt "new-password" "P" "New user passord" InvisibleRepeat
  ) $ \newPassword uid bck -> do
      either (return . responseFail . UserStorageBackendError)
             (const $ return responseOk) =<<
               updateUser bck uid (\old -> old
                 { u_password = makePassword $ PasswordPlain newPassword
                 } :: User UserData)

commands :: UserStorageBackend bck => [(T.Text, Command bck (IO Response))]
commands =
  [ cmdCreateUser
  , cmdUpdateUser
  , cmdUpdatePassword
  , cmdDeleteUser
  , cmdResetPassword
  , cmdApplyPassword
  ]
