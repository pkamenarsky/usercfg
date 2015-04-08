{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TemplateHaskell #-}

module Command where

import           Control.Applicative
import           Control.Monad

import           Data.Aeson.TH
import qualified Data.Text as T
import           Data.Maybe

import           Data.Tuple.Curry
import           Data.Tuple.Sequence

import           Web.Users.Types
import           Web.PathPieces

import           Model

data Request = Request
  { rqCommand :: T.Text
  , rqOptions :: [(T.Text, T.Text)]
  } deriving (Eq, Show)

deriveJSON (opts { fieldLabelModifier     = rmvPrefix "rq"
                 , constructorTagModifier = rmvPrefix ""}) ''Request

data Response =
    Ok
  | Fail
    { rspMessage  :: T.Text
    } deriving (Eq, Show)

deriveJSON (opts { fieldLabelModifier     = rmvPrefix "rsp"
                 , constructorTagModifier = rmvPrefix ""}) ''Response

data Option a b = Option
  { optName     :: T.Text
  , optDesc     :: T.Text
  , optDefault  :: b
  } deriving (Eq, Show)

type Keys = [(T.Text, T.Text)]

data Resolve a = Resolve { unRes :: (Keys -> a) } deriving Functor

class Read a => Lookupable a b where
  llkup :: Read a => [(T.Text, T.Text)] -> Option a b -> Maybe a

instance Read a => Lookupable a () where
  llkup opts (Option {..}) = (read . T.unpack) <$> lookup optName opts

instance Read a => Lookupable (Maybe a) (Maybe a) where
  llkup opts (Option {..}) = ((read . T.unpack) <$> lookup optName opts) <|> Just optDefault

opt :: Lookupable a b => Option a b -> Resolve a
opt o = Resolve $ \keys -> fromJust $ llkup keys o

instance Applicative Resolve where
  pure a = Resolve $ const a
  (Resolve f) <*> (Resolve b) = Resolve $ \keys -> f keys $ b keys

instance Monad Resolve where
  return = pure
  Resolve a >>= f = Resolve $ \keys -> unRes (f $ a keys) keys

type Opt a = Option a ()
type OptMay a = Option (Maybe a) (Maybe a)

apply :: (Monad m, SequenceT a (m b), Curry (b -> c) d)  => a -> d -> m c
apply opts f = sequenceT opts >>= return . uncurryN f
