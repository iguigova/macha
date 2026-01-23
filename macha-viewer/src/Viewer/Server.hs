{-# LANGUAGE OverloadedStrings #-}
module Viewer.Server (runServer) where

import Web.Scotty (scotty, middleware, get, post, json, raw, setHeader, captureParam, body, liftIO, ActionM)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import System.FilePath (takeDirectory, (</>))

import Viewer.Markdown (listFiles, renderFile, moveFile, updateNotes, dirStats)
import Viewer.Types () -- ToJSON instances

-- Sibling directory names
doneDir :: FilePath
doneDir = "done"

discardedDir :: FilePath
discardedDir = "discarded"

queueDir :: FilePath
queueDir = "queue"

applicationsDir :: FilePath
applicationsDir = "applications"

okResponse :: Value
okResponse = object ["status" .= ("ok" :: Text)]

resolveDir :: FilePath -> ActionM FilePath
resolveDir parentDir = do
  dirParam <- captureParam "dir" :: ActionM Text
  pure $ parentDir </> T.unpack dirParam

runServer :: Int -> FilePath -> Text -> IO ()
runServer port dir indexHtml = do
  putStrLn $ "Macha Viewer running on http://localhost:" <> show port
  putStrLn $ "Serving: " <> dir
  let parentDir = takeDirectory dir
  scotty port $ do
    middleware logStdoutDev

    get "/" $ do
      setHeader "Content-Type" "text/html; charset=utf-8"
      raw . LTE.encodeUtf8 . LT.fromStrict $ indexHtml

    get "/api/:dir/files" $ do
      targetDir <- resolveDir parentDir
      appFiles <- liftIO $ listFiles targetDir
      json appFiles

    get "/api/:dir/file/:name" $ do
      targetDir <- resolveDir parentDir
      name <- captureParam "name" :: ActionM Text
      rendered <- liftIO $ renderFile targetDir name
      setHeader "Content-Type" "text/html; charset=utf-8"
      raw . LTE.encodeUtf8 . LT.fromStrict $ rendered

    post "/api/:dir/file/:name/apply" $ do
      targetDir <- resolveDir parentDir
      name <- captureParam "name" :: ActionM Text
      liftIO $ moveFile targetDir name (parentDir </> doneDir)
      json okResponse

    post "/api/:dir/file/:name/discard" $ do
      targetDir <- resolveDir parentDir
      name <- captureParam "name" :: ActionM Text
      liftIO $ moveFile targetDir name (parentDir </> discardedDir)
      json okResponse

    post "/api/:dir/file/:name/notes" $ do
      targetDir <- resolveDir parentDir
      name <- captureParam "name" :: ActionM Text
      reqBody <- body
      let notes = TE.decodeUtf8 (BL.toStrict reqBody)
      liftIO $ updateNotes targetDir name (T.strip notes)
      json okResponse

    get "/api/stats" $ do
      stats <- liftIO $ do
        apps <- dirStats (parentDir </> applicationsDir)
        done <- dirStats (parentDir </> doneDir)
        disc <- dirStats (parentDir </> discardedDir)
        queue <- dirStats (parentDir </> queueDir)
        pure $ object
          [ "applications" .= apps
          , "done"         .= done
          , "discarded"    .= disc
          , "queue"        .= queue
          ]
      json stats
