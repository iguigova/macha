{-# LANGUAGE OverloadedStrings #-}
module Viewer.Markdown
  ( listFiles
  , renderFile
  , parseMetadata
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import CMarkGFM (CMarkExtension, commonmarkToHtml, optUnsafe, extTable, extStrikethrough, extAutolink)
import Viewer.Types (FileInfo(..))

-- Markdown field markers
headingPrefix :: Text
headingPrefix = "# "

urlFieldMarker :: Text
urlFieldMarker = "**URL:**"

fitFieldMarker :: Text
fitFieldMarker = "**Fit:**"

-- GFM extensions enabled for rendering
gfmExtensions :: [CMarkExtension]
gfmExtensions = [extTable, extStrikethrough, extAutolink]

listFiles :: FilePath -> IO [FileInfo]
listFiles dir = do
  entries <- listDirectory dir
  let mdFiles = filter isMd entries
  mapM (parseFile dir) mdFiles

isMd :: FilePath -> Bool
isMd f = takeExtension f == ".md"

parseFile :: FilePath -> FilePath -> IO FileInfo
parseFile dir filename =
  parseMetadata (T.pack filename) <$> TIO.readFile (dir </> filename)

parseMetadata :: Text -> Text -> FileInfo
parseMetadata filename content =
  let ls = T.lines content
      title = case filter (T.isPrefixOf headingPrefix) ls of
                (l:_) -> T.strip (T.drop (T.length headingPrefix) l)
                []    -> filename
      url = extractField urlFieldMarker ls
      fit = extractField fitFieldMarker ls
  in FileInfo
    { fiFilename = filename
    , fiTitle    = title
    , fiUrl      = url
    , fiFit      = fit
    }

extractField :: Text -> [Text] -> Text
extractField prefix ls =
  case filter (T.isPrefixOf prefix) ls of
    (l:_) -> T.strip (T.drop (T.length prefix) l)
    []    -> ""

renderFile :: FilePath -> Text -> IO Text
renderFile dir filename = do
  content <- TIO.readFile (dir </> T.unpack filename)
  pure $ commonmarkToHtml [optUnsafe] gfmExtensions content
