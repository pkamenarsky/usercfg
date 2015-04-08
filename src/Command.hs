{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TemplateHaskell #-}

module Command where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader

import           Data.Aeson.TH
import qualified Data.Text as T
import           Data.Maybe

import           Data.Tuple.Curry
import           Data.Tuple.Sequence

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

data Command opts bck r = Command
  { cmdOpts :: opts
  , cmdFn   :: Resolve (bck -> IO r)
  }

apply :: (Monad m, SequenceT a (m b), Curry (b -> c) d)  => a -> d -> m c
apply opts f = sequenceT opts >>= return . uncurryN f

-- cmd :: opts -> bck -> f -> Command opts bck r
cmd cmdOpts f = Command { cmdFn = optResolve $ apply cmdOpts f, .. }
