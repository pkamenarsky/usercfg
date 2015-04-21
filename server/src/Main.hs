{-# LANGUAGE CPP, DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, RecordWildCards, OverloadedStrings, TemplateHaskell, TypeOperators #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Either
import           Control.Monad.Reader

import           Crypto.PubKey.OpenSsh
import           Crypto.PubKey.HashDescr
import           Crypto.PubKey.RSA.PKCS15
import           Crypto.PubKey.DH
import           Crypto.Random
import           Crypto.Types.PubKey.DH

import           Data.Aeson
import qualified Data.ByteString          as B
import qualified Data.ByteString.Base64   as B64
import qualified Data.ByteString.Char8    as BC
import qualified Data.Cache.LRU           as LRU
import           Data.Either.Combinators
import           Data.IORef
import           Data.List
import qualified Data.Map               as M
import           Data.Maybe
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

import           Command                  (Command (..))
import           Commands
import           Model

params :: Params
params = Params
  0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6BF12FFA06D98A0864D87602733EC86A64521F2B18177B200CBBE117577A615D6C770988C0BAD946E208E24FA074E5AB3143DB5BFCE0FD108E4B82D120A92108011A723C12A787E6D788719A10BDBA5B2699C327186AF4E23C1A946834B6150BDA2583E9CA2AD44CE8DBBBC2DB04DE8EF92E8EFC141FBECAA6287C59474E6BC05D99B2964FA090C3A2233BA186515BE7ED1F612970CEE2D7AFB81BDD762170481CD0069127D5B05AA993B4EA988D8FDDC186FFB7DC90A6C08F4DF435C93402849236C3FAB4D27C7026C1D4DCB2602646DEC9751E763DBA37BDF8FF9406AD9E530EE5DB382F413001AEB06A53ED9027D831179727B0865A8918DA3EDBEBCF9B14ED44CE6CBACED4BB1BDB7F1447E6CC254B332051512BD7AF426FB8F401378CD2BF5983CA01C64B92ECF032EA15D1721D03F482D7CE6E74FEF6D55E702F46980C82B5A84031900B1C9E59E7C97FBEC7E8F323A97A7E36CC88BE0F1D45B7FF585AC54BD407B22B4154AACC8F6D7EBF48E1D814CC5ED20F8037E0A79715EEF29BE32806A1D58BB7C5DA76F550AA3D8A1FBFF0EB19CCB1A313D55CDA56C9EC2EF29632387FE8D76E3C0468043E8F663F4860EE12BF2D5B0B7474D6E694F91E6DCC4024FFFFFFFFFFFFFFFF
  2

type Api = "info" :> Get Response
      :<|> "dh" :> ReqBody DhRequest :> Post Response
      :<|> "command" :> ReqBody DhCmdRequest :> Post Response
#ifdef DEBUG
      :<|> "print_state" :> Get ()
#endif

api :: Proxy Api
api = Proxy

data DhData = DhData
  { dhLRU   :: LRU.LRU T.Text SharedKey
  , dhCPRG  :: SystemRNG
  }

type DhState = IORef DhData

mbToE :: e -> Maybe a -> Either e a
mbToE _ (Just x) = Right x
mbToE e Nothing  = Left e

mbToET :: Monad m => e -> Maybe a -> EitherT e m a
mbToET _ (Just x) = right x
mbToET e Nothing  = left e

authPubKey :: Keys -> Maybe SharedKey -> T.Text -> T.Text -> Either Error (M.Map T.Text T.Text -> Bool)
authPubKey keys shared sshHash sigBlob = do
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
                       <*> pure ((BC.pack $ show shared') `B.append` keysHash keys)
                       <*> pure sig
    where
      unpackShared (Just (SharedKey x)) = Right x
      unpackShared _                    = Left NoSharedKeyError

      unpackPubKey (Right (OpenSshPublicKeyRsa x _)) = Just x
      unpackPubKey _                                 = Nothing

keysHash :: Keys -> B.ByteString
keysHash = TE.encodeUtf8 . T.concat . sort . map (uncurry T.append)

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

           let (priv, cprg)       = generatePrivate (dhCPRG st) params
               PublicNumber svPub = calculatePublic params priv
               shared             = getShared params priv (PublicNumber dhClPub)

           modifyIORef stref $ \st' -> st'
             { dhLRU  = LRU.insert dhReqUser shared $ dhLRU st'
             , dhCPRG = cprg
             }

           return $ response $ toJSON svPub

        command DhCmdRequest {..} = liftIO $ do
          st <- readIORef stref

          let (lru', shared) = LRU.delete dhClUser $ dhLRU st

              mbToR r = maybe (return $ responseFail r)

              auth cmd
                | Just clPass <- dhClPass = do
                    r <- withAuthUser bck dhClUser (PasswordPlain clPass) cmd
                    mbToR AuthError return r
                | Just (keyHash, sig) <- dhClSig = runEitherT $ do
                    f <- hoistEither $ authPubKey dhClOptions shared keyHash sig
                    r <- liftIO
                       $ withAuthUserByUserData bck dhClUser (f . usrSshKeys) cmd
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
