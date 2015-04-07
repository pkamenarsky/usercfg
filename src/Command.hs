{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RecordWildCards #-}

module Command where

import           Control.Applicative

import qualified Data.Text as T
import           Data.Maybe

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

class Exec opt optLk where
  exec :: [(T.Text, T.Text)] -> Command opt optLk -> IO ()

lkp :: Read a => [(T.Text, T.Text)] -> Option a -> Maybe a
lkp opts (Option {..}) = ((read . T.unpack) <$> lookup optName opts) <|> optDefault

instance (Read a, Read b, Read c, Read d, Read e) =>
         Exec (Option a, Option b, Option c, Option d, Option e)
         (a -> b -> c -> d -> e -> IO ()) where
  exec os (Command {..}) = fromMaybe (return ()) $ do
    let (a, b, c, d, e) = cmdOptions
    cmdFn <$> lkp os a <*> lkp os b <*> lkp os c <*> lkp os d <*> lkp os e

