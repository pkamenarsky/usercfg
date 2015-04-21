{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, RecordWildCards, TemplateHaskell, TypeFamilies #-}

module Command where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader

import           Data.Aeson
import           Data.Either.Combinators  (isLeft)
import qualified Data.Text                as T

import           Data.Tuple.Curry
import           Data.Tuple.OneTuple
import           Data.Tuple.Sequence

import           Web.Users.Types

import           Model

type Keys = [(T.Text, T.Text)]

type Resolve a = ReaderT Keys Maybe a

data Prompt = None | Prompt | Invisible

data Option a = Option
  { optName     :: T.Text
  , optShort    :: T.Text
  , optDesc     :: T.Text
  , optDefault  :: Maybe a
  , optPrompt   :: Prompt
  , optResolve  :: Resolve a
  } deriving Functor

instance ToJSON Prompt where
  toJSON None      = "none"
  toJSON Prompt    = "prompt"
  toJSON Invisible = "invisible"

instance ToJSON a => ToJSON (Option a) where
  toJSON Option {..}
    | optName == "" = Null
    | otherwise = object'
      [ "name"        .= optName
      , "short"       .= optShort
      , "description" .= optDesc
      , "default"     .= optDefault
      , "prompt"      .= optPrompt
      ]

emptyOption :: Option a
emptyOption = Option { optName = "", optShort = "", optDesc = "", optPrompt = None, optDefault = Nothing, optResolve = undefined }

instance Applicative Option where
  pure  = return
  (<*>) = ap

instance Monad Option where
  return x = emptyOption { optResolve = return x }
  v >>= f  = emptyOption { optResolve = optResolve v >>= (optResolve <$> f) }

opt :: Read a => T.Text -> T.Text -> T.Text -> Prompt -> Option a
opt optName optShort optDesc optPrompt = Option
  { optResolve = ReaderT $ \keys -> read . T.unpack <$> lookup optName keys
  , optDefault = Nothing
  , ..
  }

optMay :: Read a => T.Text -> T.Text -> T.Text -> Prompt -> Maybe a -> Option (Maybe a)
optMay optName optShort optDesc optPrompt optDefault' = Option
  { optResolve = ReaderT $ \keys -> (read . T.unpack <$> lookup optName keys) <|> Just optDefault'
  , optDefault = Just optDefault'
  , ..
  }

data Command bck r = forall opts. ToJSON opts => Command
  { cmdName     :: T.Text
  , cmdConfirm  :: Bool
  , cmdOpts     :: opts
  , cmdFn       :: Either (Resolve (bck -> r)) (Resolve (UserId bck -> bck -> r))
  }

instance ToJSON (Command bck r) where
  toJSON (Command {..}) = object'
    [ "name"    .= cmdName
    , "options" .= cmdOpts
    , "confirm" .= cmdConfirm
    , "auth"    .= isLeft cmdFn
    ]

instance FromJSON a => FromJSON (OneTuple a) where
  parseJSON a = OneTuple <$> parseJSON a

instance ToJSON a => ToJSON (OneTuple a) where
  toJSON (OneTuple a) = toJSON a

noArgs :: Data.Tuple.OneTuple.OneTuple (Option (Maybe ()))
noArgs = OneTuple $ optMay "" "" "" None Nothing

apply :: (Monad m, SequenceT a (m b), Curry (b -> c) d)  => a -> d -> m c
apply opts f = sequenceT opts >>= return . uncurryN f

cmd :: (ToJSON opts, SequenceT opts (Option b), Curry (b -> bck -> r) f) => T.Text -> Bool -> opts -> f -> (T.Text, Command bck r)
cmd cmdName cmdConfirm cmdOpts f = (cmdName, Command { cmdFn = Left $ optResolve $ apply cmdOpts f, .. })

cmdAuth :: (ToJSON opts, SequenceT opts (Option b), Curry (b -> UserId bck -> bck -> r) f) => T.Text -> Bool -> opts -> f -> (T.Text, Command bck r)
cmdAuth cmdName cmdConfirm cmdOpts f = (cmdName, Command { cmdFn = Right $ optResolve $ apply cmdOpts f, .. })
