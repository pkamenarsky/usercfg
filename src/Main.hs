module Main where

import qualified Data.Text as T

data Request =
    Request
    { rqCommand :: T.Text
    , rqOptions :: [T.Text] -- key-value map?
    }
  | RequestCnt
    { rqToken   :: T.Text
    , rqAnswer  :: T.Text
    , rqOptions :: [T.Text]
    } deriving (Eq, Show)

data Response =
    Ok
  | Fail
    { rspMessage  :: T.Text
    }
  | Question
    { rspQuestion :: Question
    , rspToken    :: T.Text
    } deriving (Eq, Show)

data Question =
  | QuestionYN
  | QuestionOption
    { rspOptions :: [T.Text]
    } deriving (Eq, Show)

main :: IO ()
main = print "asd"
