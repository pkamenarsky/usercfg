{-# LANGUAGE CPP, DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, RecordWildCards, OverloadedStrings, TemplateHaskell, TypeOperators #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Either
import           Control.Monad.Reader

import qualified Crypto.Hash.SHA1         as H
import           Crypto.PubKey.OpenSsh
import           Crypto.PubKey.HashDescr
import           Crypto.PubKey.RSA.PKCS15
import           Crypto.Random

import           Data.Aeson
import qualified Data.ByteString          as B
import qualified Data.ByteString.Base16   as B16
import qualified Data.ByteString.Base64   as B64
import qualified Data.ByteString.Char8    as BC
import qualified Data.Cache.LRU           as LRU
import           Data.Either.Combinators
import           Data.IORef
import qualified Data.Map                 as M
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Serialize.Get       (getWord32be, getWord8, runGet)
import qualified Data.Text.Encoding       as TE

import           Database.PostgreSQL.Simple

import           Servant.API
import           Servant.Server

import           Web.Users.Types
import           Web.Users.Postgresql     ()

import qualified Data.Text                as T

import           Network.Wai.Handler.Warp

import           System.Environment

import           Command                  (Command (..), Option (..), userOption, passOption)
import           Commands
import           Model

#ifdef DEBUG
import           Debug.Trace

trc :: Show a => String -> a -> a
trc str x = trace (str ++ ": " ++ show x) x
#endif

type Api = "info" :> Get Response
      :<|> "dh" :> ReqBody DhRequest :> Post Response
      :<|> "command" :> ReqBody DhCmdRequest :> Post Response
#ifdef DEBUG
      :<|> "print_state" :> Get ()
#endif

api :: Proxy Api
api = Proxy

data DhData = DhData
  { dhLRU   :: LRU.LRU T.Text B.ByteString
  , dhCPRG  :: SystemRNG
  }

type DhState = IORef DhData

mbToE :: e -> Maybe a -> Either e a
mbToE _ (Just x) = Right x
mbToE e Nothing  = Left e

mbToET :: Monad m => e -> Maybe a -> EitherT e m a
mbToET _ (Just x) = right x
mbToET e Nothing  = left e

authPubKey :: DhCmdRequest -> Maybe B.ByteString -> T.Text -> T.Text -> Either Error (M.Map T.Text T.Text -> Bool)
authPubKey req shared sshHash sigBlob = do
  shared'  <- unpackShared shared
  blob     <- mapLeft (const $ ParseError "b64") $ B64.decode $ TE.encodeUtf8 sigBlob

  (_, sig) <- mapLeft ParseError $ flip runGet blob $ do
    al   <- fromIntegral <$> getWord32be
    algo <- B.pack <$> replicateM al getWord8
    sl   <- fromIntegral <$> getWord32be
    sig  <- B.pack <$> replicateM sl getWord8
    return (algo, sig)

  Right $ \sshKeys ->
      let pubkey  = M.lookup sshHash sshKeys
          pubkey' = join $ unpackPubKey . decodePublic . TE.encodeUtf8 <$> pubkey
      in fromMaybe False $ verify hashDescrSHA1
                       <$> pubkey'
                       <*> pure (shared' <> hashCmdRequest req)
                       <*> pure sig
    where
      unpackShared (Just x) = Right x
      unpackShared _        = Left NoSharedKeyError

      unpackPubKey (Right (OpenSshPublicKeyRsa x _)) = Just x
      unpackPubKey _                                 = Nothing

runServer :: UserStorageBackend bck => Int -> bck -> [(T.Text, Command bck (IO Response))] -> IO ()
runServer port bck cmds = do
  ep    <- createEntropyPool
  stref <- newIORef $ DhData (LRU.newLRU $ Just 10000) (cprgCreate ep)

  initUserBackend bck

  runServer' stref
  where
    runServer' stref = run port $ serve api $ info
                                         :<|> dh
                                         :<|> command
#ifdef DEBUG
                                         :<|> printState
#endif
      where
        info = return $ response $ toJSON cmds

        dh DhRequest {..} = liftIO $ do
           st <- readIORef stref

           let (shared, cprg) = cprgGenerate 256 (dhCPRG st)

           modifyIORef stref $ \st' -> st'
             { dhLRU  = LRU.insert (T.take 40 dhReqHash) shared $ dhLRU st'
             , dhCPRG = cprg
             }

           return $ response $ toJSON $ BC.unpack $ B64.encode shared

        command req@(DhCmdRequest {..}) = liftIO $ do
          st <- readIORef stref

          let userMay = lookup (optName userOption) dhClOptions
              passMay = lookup (optName passOption) dhClOptions
              cmdHash = TE.decodeUtf8 $ B16.encode $ H.hash $ hashCmdRequest req
              (lru', shared) = LRU.delete cmdHash $ dhLRU st

              mbToR r = maybe (return $ responseFail r)

              auth cmd
                | Just pass <- passMay
                , Just user <- userMay = do
                    r <- withAuthUser bck user (PasswordPlain pass) cmd
                    mbToR AuthError return r
                | Just (keyHash, sig) <- dhClSig
                , Just user           <- userMay = runEitherT $ do
                    f <- hoistEither $ authPubKey req shared keyHash sig
                    r <- liftIO
                       $ withAuthUserByUserData bck user (f . usrSshKeys) cmd
                    hoistEither $ maybe (Left AuthError) id r
                | otherwise = return $ responseFail AuthNeededError

              exec cmd
                | Left f  <- cmdFn cmd =
                    mbToR MissingOptionsError ($ bck) $ runReaderT f dhClOptions
                | Right f <- cmdFn cmd =
                    auth $ \uid -> mbToR MissingOptionsError (\rsv -> rsv uid bck)
                         $ runReaderT f dhClOptions
                | otherwise = return $ responseFail NoSuchCommandError

              execCmd
                | Just cmd <- lookup dhClCommand cmds
                  = exec cmd
                | otherwise
                  = return $ responseFail NoSuchCommandError

          writeIORef stref $ st { dhLRU = lru' }

          execCmd

#ifdef DEBUG
        printState = liftIO $ do
          readIORef stref >>= print . dhLRU
          return ()
#endif

main :: IO ()
main = do
  dburl <- fromMaybe "" <$> lookupEnv "DATABASE_URL"
  port <- read . fromMaybe "8000" <$> lookupEnv "PORT"

  bck <- connectPostgreSQL $ BC.pack dburl
  housekeepBackend bck

  runServer port bck commands
