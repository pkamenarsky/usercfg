{-# LANGUAGE ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TemplateHaskell #-}

module Command where

import           Control.Applicative

import           Data.Aeson.TH
import qualified Data.Text as T
import           Data.Maybe

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

type Opt a = Option a ()
type OptMay a = Option (Maybe a) (Maybe a)

data Command bck opt optLk = Command
  { cmdName     :: T.Text
  , cmdOptions  :: opt
  , cmdFn       :: bck -> optLk
  }

instance Show (Command bck a b) where
  show = show . cmdName

class Read a => Lookupable a b where
  llkup :: Read a => [(T.Text, T.Text)] -> Option a b -> Maybe a

instance Read a => Lookupable a () where
  llkup opts (Option {..}) = (read . T.unpack) <$> lookup optName opts

instance Read a => Lookupable (Maybe a) (Maybe a) where
  llkup opts (Option {..}) = ((read . T.unpack) <$> lookup optName opts) <|> Just optDefault

class Exec bck opt optLk where
  exec :: Command bck opt optLk -> bck -> [(T.Text, T.Text)] -> IO Response

instance (Lookupable a d1, Lookupable b d2, Lookupable c d3, Lookupable d d4, Lookupable e d5) =>
         Exec bck (Option a d1, Option b d2, Option c d3, Option d d4, Option e d5)
                  (a -> b -> c -> d -> e -> IO Response) where
  exec (Command {..}) bck os = fromMaybe (return $ Fail "*** FIXME ERROR") $ do
    let (a, b, c, d, e) = cmdOptions
    cmdFn bck <$> llkup os a <*> llkup os b <*> llkup os c <*> llkup os d <*> llkup os e
