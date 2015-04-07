{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RecordWildCards #-}

module Command where

import           Control.Applicative

import qualified Data.Text as T
import           Data.Maybe

import           Web.Users.Types

data Option a = Option
  { optName     :: T.Text
  , optDesc     :: T.Text
  , optOptional :: Bool
  , optDefault  :: Maybe a
  }

data Command opt optLk = Command
  { cmdName     :: T.Text
  , cmdOptions  :: opt
  , cmdFn       :: optLk
  }

class Exec bck opt optLk where
  exec :: bck -> [(T.Text, T.Text)] -> Command opt optLk -> IO ()

lkp :: Read a => [(T.Text, T.Text)] -> Option a -> Maybe a
lkp opts (Option {..}) = ((read . T.unpack) <$> lookup optName opts) <|> optDefault

instance (Read a, Read b, Read c, Read d, Read e, UserStorageBackend bck) =>
         Exec bck (Option a, Option b, Option c, Option d, Option e)
         (bck -> a -> b -> c -> d -> e -> IO ()) where
  exec bck os (Command {..}) = fromMaybe (return ()) $ do
    let (a, b, c, d, e) = cmdOptions
    cmdFn bck <$> lkp os a <*> lkp os b <*> lkp os c <*> lkp os d <*> lkp os e

