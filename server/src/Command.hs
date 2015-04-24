{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, RecordWildCards, TemplateHaskell, TypeFamilies #-}

module Command where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader

import           Data.Aeson
import           Data.Either.Combinators  (isRight)
import qualified Data.Text                as T

import           Data.Tuple.Curry
import           Data.Tuple.OneTuple
import           Data.Tuple.Sequence

import qualified Data.Vector              as V

import           Web.Users.Types

import           Model

type Resolve a = ReaderT Keys Maybe a

data Prompt = None | Prompt | Invisible | PromptRepeat | InvisibleRepeat

data Option a = Option
  { optName     :: T.Text
  , optShort    :: T.Text
  , optDesc     :: T.Text
  , optDefault  :: Maybe a
  , optRequired :: Bool
  , optPrompt   :: Prompt
  , optResolve  :: Resolve a
  } deriving Functor

instance ToJSON Prompt where
  toJSON None            = Null
  toJSON Prompt          = toJSON ("prompt" :: T.Text, False)
  toJSON Invisible       = toJSON ("invisible" :: T.Text, False)
  toJSON PromptRepeat    = toJSON ("prompt" :: T.Text, True)
  toJSON InvisibleRepeat = toJSON ("invisible" :: T.Text, True)

instance ToJSON a => ToJSON (Option a) where
  toJSON Option {..}
    | optName == "" = Null
    | otherwise = object'
      [ "name"        .= optName
      , "short"       .= optShort
      , "description" .= optDesc
      , "default"     .= optDefault
      , "required"    .= optRequired
      , "prompt"      .= optPrompt
      ]

emptyOption :: Option a
emptyOption = Option { optName = "", optShort = "", optDesc = "", optRequired = False, optPrompt = None, optDefault = Nothing, optResolve = undefined }

userOption :: Option T.Text
userOption = opt "name" "n" "User name" None

passOption :: Option T.Text
passOption = opt "password" "p" "User password" InvisibleRepeat

passOptionMay :: Option (Maybe T.Text)
passOptionMay = optMay "password" "p" "User password" Invisible Nothing

instance Applicative Option where
  pure  = return
  (<*>) = ap

instance Monad Option where
  return x = emptyOption { optResolve = return x }
  v >>= f  = emptyOption { optResolve = optResolve v >>= (optResolve <$> f) }

class Readable a where
  read' :: T.Text -> Maybe a

instance Readable () where
  read' "" = Just ()
  read' _  = Nothing

instance Readable T.Text where
  read' = Just

instance Readable a => Readable (Maybe a) where
  read' ""  = Nothing
  read' str = Just $ read' str

opt :: Readable a => T.Text -> T.Text -> T.Text -> Prompt -> Option a
opt optName optShort optDesc optPrompt = Option
  { optResolve  = ReaderT $ \keys -> join $ read' <$> lookup optName keys
  , optDefault  = Nothing
  , optRequired = True
  , ..
  }

optMay :: Readable a => T.Text -> T.Text -> T.Text -> Prompt -> Maybe a -> Option (Maybe a)
optMay optName optShort optDesc optPrompt optDefault' = Option
  { optResolve  = ReaderT $ \keys -> (join $ read' <$> lookup optName keys) <|> Just optDefault'
  , optDefault  = Just optDefault'
  , optRequired = False
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
    , "options" .= (amendOpts $ toJSON cmdOpts)
    , "confirm" .= cmdConfirm
    , "auth"    .= isRight cmdFn
    ]
    where
      amendOpts os@(Array a)
        | isRight cmdFn = Array (toJSON userOption `V.cons` (toJSON passOptionMay `V.cons` a))
        | otherwise     = os
      amendOpts os      = os

instance FromJSON a => FromJSON (OneTuple a) where
  parseJSON (Array a) = OneTuple <$> (parseJSON $ V.head a)
  parseJSON _         = fail "Expected array when parsing tuple"

instance ToJSON (Option a) => ToJSON (OneTuple (Option a)) where
  toJSON (OneTuple o@(Option {..})) | T.null optName = toJSON ([] :: [Int])
                                    | otherwise      = toJSON [o]

noArgs :: Data.Tuple.OneTuple.OneTuple (Option (Maybe ()))
noArgs = OneTuple $ optMay "" "" "" None Nothing

apply :: (Monad m, SequenceT a (m b), Curry (b -> c) d)  => a -> d -> m c
apply opts f = sequenceT opts >>= return . uncurryN f

cmd :: (ToJSON opts, SequenceT opts (Option b), Curry (b -> bck -> r) f) => T.Text -> Bool -> opts -> f -> (T.Text, Command bck r)
cmd cmdName cmdConfirm cmdOpts f = (cmdName, Command { cmdFn = Left $ optResolve $ apply cmdOpts f, .. })

cmdAuth :: (ToJSON opts, SequenceT opts (Option b), Curry (b -> UserId bck -> bck -> r) f) => T.Text -> Bool -> opts -> f -> (T.Text, Command bck r)
cmdAuth cmdName cmdConfirm cmdOpts f = (cmdName, Command { cmdFn = Right $ optResolve $ apply cmdOpts f, .. })
