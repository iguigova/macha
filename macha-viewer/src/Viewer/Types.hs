{-# LANGUAGE OverloadedStrings #-}
module Viewer.Types where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)

data FileInfo = FileInfo
  { fiFilename  :: !Text
  , fiTitle     :: !Text
  , fiUrl       :: !Text
  , fiFit       :: !Text
  , fiTimestamp :: !Int
  } deriving (Show, Eq)

instance ToJSON FileInfo where
  toJSON fi = object
    [ "filename"  .= fiFilename fi
    , "title"     .= fiTitle fi
    , "url"       .= fiUrl fi
    , "fit"       .= fiFit fi
    , "timestamp" .= fiTimestamp fi
    ]

data DirStats = DirStats
  { dsTotal :: !Int
  , dsToday :: !Int
  , dsWeek  :: !Int
  , dsMonth :: !Int
  } deriving (Show, Eq)

instance ToJSON DirStats where
  toJSON ds = object
    [ "total" .= dsTotal ds
    , "today" .= dsToday ds
    , "week"  .= dsWeek ds
    , "month" .= dsMonth ds
    ]
