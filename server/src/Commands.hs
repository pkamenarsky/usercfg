{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Commands where

import qualified Crypto.Hash.SHA1       as H

import qualified Data.ByteString.Base64 as B64
import qualified Data.Map               as M
import           Data.Proxy
import qualified Data.Text.Encoding     as TE

import           Web.Users.Types

import           Model
import           Command

cmds :: UserStorageBackend bck => Proxy bck -> [Command bck (IO Response)]
cmds _ =
  [ cmd "create-user" False
    ( opt    "name" "User name"
    , opt    "email" "User mail"
    , opt    "password" "User password"
    , optMay "number" "User number" Nothing
    , optMay "ssh-key" "SSH public key" Nothing
    ) $ \u_name u_email password usrNumber sshKey bck -> do
        let sshKeyHash =  maybe "" (TE.decodeUtf8 . B64.encode . H.hash . TE.encodeUtf8) sshKey

        either (return . Fail . UserStorageBackendError) (const $ return Ok) =<<
          createUser bck (User
            { u_active = True
            , u_more   = UserData
                { usrSshKeys = maybe M.empty (M.singleton sshKeyHash) sshKey
                , ..
                }
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

