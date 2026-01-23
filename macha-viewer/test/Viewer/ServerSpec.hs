{-# LANGUAGE OverloadedStrings #-}
module Viewer.ServerSpec (spec) where

import Test.Hspec
import qualified Data.Text.IO as TIO
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import Network.Wai (Application)
import Network.Wai.Test (runSession, request, simpleStatus, simpleBody, setPath, defaultRequest)
import Network.HTTP.Types (status200)
import Web.Scotty (scottyApp, get, setHeader, raw, json, liftIO, captureParam, ActionM)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL

import Viewer.Markdown (listFiles, renderFile)
import Viewer.Types ()

mkApp :: FilePath -> Text -> IO Application
mkApp dir indexHtml = scottyApp $ do
  get "/" $ do
    setHeader "Content-Type" "text/html; charset=utf-8"
    raw . LTE.encodeUtf8 . LT.fromStrict $ indexHtml

  get "/api/files" $ do
    files <- liftIO $ listFiles dir
    json files

  get "/api/file/:name" $ do
    name <- captureParam "name" :: ActionM Text
    rendered <- liftIO $ renderFile dir name
    setHeader "Content-Type" "text/html; charset=utf-8"
    raw . LTE.encodeUtf8 . LT.fromStrict $ rendered

spec :: Spec
spec = do
  describe "GET /" $ do
    it "serves the index HTML with 200" $ do
      cwd <- getCurrentDirectory
      indexHtml <- TIO.readFile (cwd </> "static" </> "index.html")
      withSystemTempDirectory "viewer-test" $ \dir -> do
        app <- mkApp dir indexHtml
        resp <- runSession (request (setPath defaultRequest "/")) app
        simpleStatus resp `shouldBe` status200

    it "contains Macha Viewer in the HTML" $ do
      cwd <- getCurrentDirectory
      indexHtml <- TIO.readFile (cwd </> "static" </> "index.html")
      withSystemTempDirectory "viewer-test" $ \dir -> do
        app <- mkApp dir indexHtml
        resp <- runSession (request (setPath defaultRequest "/")) app
        BL.toStrict (simpleBody resp) `shouldSatisfy` \body ->
          "Macha Viewer" `T.isInfixOf` TE.decodeUtf8 body

  describe "GET /api/files" $ do
    it "returns JSON array with file info" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        TIO.writeFile (dir </> "test.md") "# Test Co - Dev\n**Fit:** Good fit\n**URL:** http://x.com"
        app <- mkApp dir "<html></html>"
        resp <- runSession (request (setPath defaultRequest "/api/files")) app
        simpleStatus resp `shouldBe` status200
        let body = TE.decodeUtf8 (BL.toStrict (simpleBody resp))
        body `shouldSatisfy` T.isInfixOf "Test Co - Dev"
        body `shouldSatisfy` T.isInfixOf "Good fit"

    it "returns empty array for empty directory" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        app <- mkApp dir "<html></html>"
        resp <- runSession (request (setPath defaultRequest "/api/files")) app
        BL.toStrict (simpleBody resp) `shouldBe` "[]"

  describe "GET /api/file/:name" $ do
    it "renders markdown to HTML" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        TIO.writeFile (dir </> "job.md") "# Hello World\n\nSome text."
        app <- mkApp dir "<html></html>"
        resp <- runSession (request (setPath defaultRequest "/api/file/job.md")) app
        simpleStatus resp `shouldBe` status200
        let body = TE.decodeUtf8 (BL.toStrict (simpleBody resp))
        body `shouldSatisfy` T.isInfixOf "<h1>"
        body `shouldSatisfy` T.isInfixOf "Hello World"
