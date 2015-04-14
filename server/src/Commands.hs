{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Commands where

import           Control.Monad.Reader

import qualified Crypto.Hash.SHA1       as H

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map               as M
import           Data.Proxy
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

import           Data.Tuple.OneTuple
import           Web.Users.Types

import           Model
import           Command

cruser :: UserStorageBackend bck => (T.Text, Command bck (IO Response))
cruser = cmd "create-user" False
    ( opt    "name" "User name"
    , opt    "email" "User mail"
    , opt    "password" "User password"
    , optMay "number" "User number" Nothing
    , optMay "ssh-key" "SSH public key" Nothing
    ) $ \u_name u_email password usrNumber sshKey bck -> do
        let sshKeyHash =  maybe "" (TE.decodeUtf8 . B16.encode . H.hash . TE.encodeUtf8) sshKey

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

cmds :: UserStorageBackend bck => Proxy bck -> [(T.Text, Command bck (IO Response))]
cmds _ =
  [ cmd "create-user" False
    ( opt    "name" "User name"
    , opt    "email" "User mail"
    , opt    "password" "User password"
    , optMay "number" "User number" Nothing
    , optMay "ssh-key" "SSH public key" Nothing
    ) $ \u_name u_email password usrNumber sshKey bck -> do
        let sshKeyHash =  maybe "" (TE.decodeUtf8 . B16.encode . H.hash . TE.encodeUtf8) sshKey

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
  , cmdAuth "delete-user" True noArgs $ \_ uid bck -> deleteUser bck uid >> return Ok
  ]

exec :: UserStorageBackend bck => bck -> T.Text -> Keys -> IO Response
exec bck name opts
  | Just cmd' <- cmd, Left f <- cmdFn cmd' = maybe (return $ Fail NoSuchCommandError) ($ bck) $ runReaderT f opts
  | otherwise = return $ Fail NoSuchCommandError
  where
    cmd = lookup name (cmds $ mkProxy bck)
