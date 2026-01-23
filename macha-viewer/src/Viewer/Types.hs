{-# LANGUAGE OverloadedStrings #-}
module Viewer.Types where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)

data FileInfo = FileInfo
  { fiFilename :: !Text
  , fiTitle    :: !Text
  , fiUrl      :: !Text
  , fiFit      :: !Text
  } deriving (Show, Eq)

instance ToJSON FileInfo where
  toJSON fi = object
    [ "filename" .= fiFilename fi
    , "title"    .= fiTitle fi
    , "url"      .= fiUrl fi
    , "fit"      .= fiFit fi
    ]
