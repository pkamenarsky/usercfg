module Crypto where

import           Crypto.PubKey.OpenSsh
import           Crypto.PubKey.HashDescr
import           Crypto.PubKey.RSA.PKCS15

import qualified Data.ByteString as B

loadKey :: IO ()
loadKey = do
  bs <- B.readFile "/Users/phil/.ssh/id_rsa.pub"
  let Right (OpenSshPublicKeyRsa key _) = decodePublic bs
  print key

  f  <- B.readFile "/Users/phil/.ssh/config"
  fs <- B.readFile "/Users/phil/.ssh/config.sign"

  let v = verify hashDescrSHA256 key f fs
  print v

  return ()
