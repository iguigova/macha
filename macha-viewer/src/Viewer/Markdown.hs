{-# LANGUAGE OverloadedStrings #-}
module Viewer.Markdown
  ( listFiles
  , renderFile
  , parseMetadata
  , moveFile
  , updateNotes
  , dirStats
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory, getModificationTime, renameFile, createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import CMarkGFM (CMarkExtension, commonmarkToHtml, optUnsafe, extTable, extStrikethrough, extAutolink)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Viewer.Types (FileInfo(..), DirStats(..))

-- Markdown field markers
headingPrefix :: Text
headingPrefix = "# "

urlFieldMarker :: Text
urlFieldMarker = "**URL:**"

fitFieldMarker :: Text
fitFieldMarker = "**Fit:**"

notesHeader :: Text
notesHeader = "## Notes"

-- GFM extensions enabled for rendering
gfmExtensions :: [CMarkExtension]
gfmExtensions = [extTable, extStrikethrough, extAutolink]

-- Time thresholds in seconds
daySeconds :: Double
daySeconds = 86400

weekSeconds :: Double
weekSeconds = 604800

monthSeconds :: Double
monthSeconds = 2592000

listFiles :: FilePath -> IO [FileInfo]
listFiles dir = do
  entries <- listDirectory dir
  let mdFiles = filter isMd entries
  mapM (parseFile dir) mdFiles

isMd :: FilePath -> Bool
isMd f = takeExtension f == ".md"

parseFile :: FilePath -> FilePath -> IO FileInfo
parseFile dir filename = do
  let path = dir </> filename
  content <- TIO.readFile path
  mtime <- getModificationTime path
  let epoch = floor (utcTimeToPOSIXSeconds mtime) :: Int
  pure $ parseMetadata (T.pack filename) epoch content

parseMetadata :: Text -> Int -> Text -> FileInfo
parseMetadata filename timestamp content =
  let ls = T.lines content
      title = case filter (T.isPrefixOf headingPrefix) ls of
                (l:_) -> T.strip (T.drop (T.length headingPrefix) l)
                []    -> filename
      url = extractField urlFieldMarker ls
      fit = extractField fitFieldMarker ls
  in FileInfo
    { fiFilename  = filename
    , fiTitle     = title
    , fiUrl       = url
    , fiFit       = fit
    , fiTimestamp = timestamp
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

moveFile :: FilePath -> Text -> FilePath -> IO ()
moveFile srcDir filename destDir = do
  createDirectoryIfMissing True destDir
  let src  = srcDir </> T.unpack filename
      dest = destDir </> T.unpack filename
  renameFile src dest

updateNotes :: FilePath -> Text -> Text -> IO ()
updateNotes dir filename notes = do
  let path = dir </> T.unpack filename
  content <- TIO.readFile path
  let updated = replaceNotes content notes
  TIO.writeFile path updated

replaceNotes :: Text -> Text -> Text
replaceNotes content notes =
  let ls = T.lines content
      beforeNotes = takeWhile (not . T.isPrefixOf notesHeader) ls
      newContent = T.unlines beforeNotes <> notesHeader <> "\n\n" <> notes <> "\n"
  in newContent

dirStats :: FilePath -> IO DirStats
dirStats dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure $ DirStats 0 0 0 0
    else do
      entries <- listDirectory dir
      let mdFiles = filter isMd entries
      now <- getCurrentTime
      mtimes <- mapM (getModificationTime . (dir </>)) mdFiles
      let total = length mdFiles
          today = countWithin now daySeconds mtimes
          week  = countWithin now weekSeconds mtimes
          month = countWithin now monthSeconds mtimes
      pure $ DirStats total today week month

countWithin :: UTCTime -> Double -> [UTCTime] -> Int
countWithin now threshold = length . filter (\t -> diffUTCTime now t < realToFrac threshold)
