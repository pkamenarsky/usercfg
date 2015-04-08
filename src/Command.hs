{-# LANGUAGE ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, Rank2Types, TypeFamilies, RecordWildCards #-}

module Command where

import           Control.Applicative

import qualified Data.Text as T
import           Data.Maybe

import           Web.Users.Types
import           Web.PathPieces

data Option a b = Option
  { optName     :: T.Text
  , optDesc     :: T.Text
  , optDefault  :: b
  }

type Opt a = Option a ()
type OptMay a = Option (Maybe a) (Maybe a)

data Command opt optLk = Command
  { cmdName     :: T.Text
  , cmdOptions  :: opt
  , cmdFn       :: UserStorageBackend b => b -> optLk
  }

class Lookupable a b where
  llkup :: Read a => [(T.Text, T.Text)] -> Option a b -> Maybe a

instance Lookupable a () where
  llkup opts (Option {..}) = (read . T.unpack) <$> lookup optName opts

instance Lookupable (Maybe a) (Maybe a) where
  llkup opts (Option {..}) = ((read . T.unpack) <$> lookup optName opts) <|> Just optDefault

class Exec bck opt optLk where
  exec :: bck -> [(T.Text, T.Text)] -> Command opt optLk -> IO ()

data BE = BE

instance UserStorageBackend BE where
  type UserId BE = String

instance (Read a, Read b, Read c, Read d, Read e, UserStorageBackend bck,
         Lookupable a d1, Lookupable b d2, Lookupable c d3, Lookupable d d4, Lookupable e d5) =>
         Exec bck (Option a d1, Option b d2, Option c d3, Option d d4, Option e d5)
                  (a -> b -> c -> d -> e -> IO ()) where
  exec bck os (Command {..}) = fromMaybe (return ()) $ do
    let (a, b, c, d, e) = cmdOptions
    cmdFn bck <$> llkup os a <*> llkup os b <*> llkup os c <*> llkup os d <*> llkup os e
