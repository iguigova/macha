{-# LANGUAGE OverloadedStrings #-}
module Viewer.Server (runServer) where

import Web.Scotty (scotty, middleware, get, post, json, raw, setHeader, captureParam, body, liftIO, ActionM)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Aeson (object, (.=))
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

    get "/api/files" $ do
      appFiles <- liftIO $ listFiles dir
      json appFiles

    get "/api/file/:name" $ do
      name <- captureParam "name" :: ActionM Text
      rendered <- liftIO $ renderFile dir name
      setHeader "Content-Type" "text/html; charset=utf-8"
      raw . LTE.encodeUtf8 . LT.fromStrict $ rendered

    post "/api/file/:name/apply" $ do
      name <- captureParam "name" :: ActionM Text
      liftIO $ moveFile dir name (parentDir </> doneDir)
      json $ object ["status" .= ("ok" :: Text)]

    post "/api/file/:name/discard" $ do
      name <- captureParam "name" :: ActionM Text
      liftIO $ moveFile dir name (parentDir </> discardedDir)
      json $ object ["status" .= ("ok" :: Text)]

    post "/api/file/:name/notes" $ do
      name <- captureParam "name" :: ActionM Text
      reqBody <- body
      let notes = TE.decodeUtf8 (BL.toStrict reqBody)
      liftIO $ updateNotes dir name (T.strip notes)
      json $ object ["status" .= ("ok" :: Text)]

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
