{-# LANGUAGE OverloadedStrings #-}
module Viewer.Server (runServer) where

import Web.Scotty (scotty, middleware, get, json, raw, setHeader, captureParam, liftIO, ActionM)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Text (Text)

import Viewer.Markdown (listFiles, renderFile)
import Viewer.Types () -- ToJSON FileInfo instance

runServer :: Int -> FilePath -> Text -> IO ()
runServer port dir indexHtml = do
  putStrLn $ "Macha Viewer running on http://localhost:" <> show port
  putStrLn $ "Serving: " <> dir
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
