{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, RecordWildCards, TemplateHaskell #-}

module Command where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text            as T
import           Data.Maybe

import           Data.Tuple.Curry
import           Data.Tuple.Sequence

import qualified Data.Vector          as V

import           Web.Users.Types
import           Web.PathPieces

import           Model

type Keys = [(T.Text, T.Text)]

type Resolve a = ReaderT Keys Maybe a

data Option a = Option
  { optName     :: T.Text
  , optDesc     :: T.Text
  , optDefault  :: Maybe a
  , optResolve  :: Resolve a
  } deriving Functor

object' :: [(T.Text, Value)] -> Value
object' = object . filter (not . isDflt . snd)
  where
    isDflt Null       = True
    isDflt (Bool b)   = not b
    isDflt (Array a)  = V.null a
    isDflt _          = False

instance ToJSON a => ToJSON (Option a) where
  toJSON (Option {..}) = object'
    [ "name"        .= optName
    , "description" .= optDesc
    , "default"     .= optDefault
    ]

emptyOption :: Option a
emptyOption = Option { optName = "", optDesc = "", optDefault = Nothing, optResolve = undefined }

instance Applicative Option where
  pure  = return
  (<*>) = ap

instance Monad Option where
  return x = emptyOption { optResolve = return x }
  v >>= f  = emptyOption { optResolve = optResolve v >>= (optResolve <$> f) }

opt :: Read a => T.Text -> T.Text -> Option a
opt optName optDesc = Option
  { optResolve = ReaderT $ \keys -> read . T.unpack <$> lookup optName keys
  , optDefault = Nothing
  , ..
  }

optMay :: Read a => T.Text -> T.Text -> Maybe a -> Option (Maybe a)
optMay optName optDesc optDefault' = Option
  { optResolve = ReaderT $ \keys -> (read . T.unpack <$> lookup optName keys) <|> Just optDefault'
  , optDefault = Just optDefault'
  , ..
  }

data Command bck r = forall opts. ToJSON opts => Command
  { cmdName     :: T.Text
  , cmdConfirm  :: Bool
  , cmdOpts     :: opts
  , cmdFn       :: Resolve (bck -> r)
  }

instance ToJSON (Command bck r) where
  toJSON (Command {..}) = object
    [ "name"    .= cmdName
    , "options" .= cmdOpts
    , "confirm" .= cmdConfirm
    ]

apply :: (Monad m, SequenceT a (m b), Curry (b -> c) d)  => a -> d -> m c
apply opts f = sequenceT opts >>= return . uncurryN f

cmd :: (ToJSON opts, SequenceT opts (Option b), Curry (b -> bck -> r) f) => T.Text -> Bool -> opts -> f -> Command bck r
cmd cmdName cmdConfirm cmdOpts f = Command { cmdFn = optResolve $ apply cmdOpts f, .. }
