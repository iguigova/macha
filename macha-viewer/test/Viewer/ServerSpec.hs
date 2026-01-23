{-# LANGUAGE OverloadedStrings #-}
module Viewer.ServerSpec (spec) where

import Test.Hspec
import qualified Data.Text.IO as TIO
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>), takeDirectory)
import System.Directory (getCurrentDirectory, doesFileExist, createDirectoryIfMissing)
import Network.Wai (Application, requestMethod)
import Network.Wai.Test (runSession, request, srequest, simpleStatus, simpleBody, setPath, defaultRequest, SRequest(..))
import Network.HTTP.Types (status200, methodPost)
import Web.Scotty (scottyApp, get, post, setHeader, raw, json, liftIO, captureParam, body, ActionM)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Text (Text)
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy as BL

import Viewer.Markdown (listFiles, renderFile, moveFile, updateNotes, dirStats)
import Viewer.Types ()

mkApp :: FilePath -> Text -> IO Application
mkApp dir indexHtml = scottyApp $ do
  let parentDir = takeDirectory dir

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

  post "/api/file/:name/apply" $ do
    name <- captureParam "name" :: ActionM Text
    liftIO $ moveFile dir name (parentDir </> "done")
    json $ object ["status" .= ("ok" :: Text)]

  post "/api/file/:name/discard" $ do
    name <- captureParam "name" :: ActionM Text
    liftIO $ moveFile dir name (parentDir </> "discarded")
    json $ object ["status" .= ("ok" :: Text)]

  post "/api/file/:name/notes" $ do
    name <- captureParam "name" :: ActionM Text
    reqBody <- body
    let notes = TE.decodeUtf8 (BL.toStrict reqBody)
    liftIO $ updateNotes dir name (T.strip notes)
    json $ object ["status" .= ("ok" :: Text)]

  get "/api/stats" $ do
    stats <- liftIO $ do
      apps <- dirStats dir
      done <- dirStats (parentDir </> "done")
      disc <- dirStats (parentDir </> "discarded")
      queue <- dirStats (parentDir </> "queue")
      pure $ object
        [ "applications" .= apps
        , "done"         .= done
        , "discarded"    .= disc
        , "queue"        .= queue
        ]
    json stats

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
        BL.toStrict (simpleBody resp) `shouldSatisfy` \b ->
          "Macha Viewer" `T.isInfixOf` TE.decodeUtf8 b

  describe "GET /api/files" $ do
    it "returns JSON array with file info" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        TIO.writeFile (dir </> "test.md") "# Test Co - Dev\n**Fit:** Good fit\n**URL:** http://x.com"
        app <- mkApp dir "<html></html>"
        resp <- runSession (request (setPath defaultRequest "/api/files")) app
        simpleStatus resp `shouldBe` status200
        let respBody = TE.decodeUtf8 (BL.toStrict (simpleBody resp))
        respBody `shouldSatisfy` T.isInfixOf "Test Co - Dev"
        respBody `shouldSatisfy` T.isInfixOf "Good fit"

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
        let respBody = TE.decodeUtf8 (BL.toStrict (simpleBody resp))
        respBody `shouldSatisfy` T.isInfixOf "<h1>"
        respBody `shouldSatisfy` T.isInfixOf "Hello World"

  describe "POST /api/file/:name/apply" $ do
    it "moves file to done directory" $
      withSystemTempDirectory "viewer-test" $ \tmpDir -> do
        let dir = tmpDir </> "applications"
        createDirectoryIfMissing True dir
        TIO.writeFile (dir </> "job.md") "# Test Job"
        app <- mkApp dir "<html></html>"
        let req = SRequest (setPath defaultRequest { requestMethod = methodPost } "/api/file/job.md/apply") ""
        resp <- runSession (srequest req) app
        simpleStatus resp `shouldBe` status200
        srcExists <- doesFileExist (dir </> "job.md")
        destExists <- doesFileExist (tmpDir </> "done" </> "job.md")
        srcExists `shouldBe` False
        destExists `shouldBe` True

  describe "POST /api/file/:name/discard" $ do
    it "moves file to discarded directory" $
      withSystemTempDirectory "viewer-test" $ \tmpDir -> do
        let dir = tmpDir </> "applications"
        createDirectoryIfMissing True dir
        TIO.writeFile (dir </> "job.md") "# Test Job"
        app <- mkApp dir "<html></html>"
        let req = SRequest (setPath defaultRequest { requestMethod = methodPost } "/api/file/job.md/discard") ""
        resp <- runSession (srequest req) app
        simpleStatus resp `shouldBe` status200
        srcExists <- doesFileExist (dir </> "job.md")
        destExists <- doesFileExist (tmpDir </> "discarded" </> "job.md")
        srcExists `shouldBe` False
        destExists `shouldBe` True

  describe "POST /api/file/:name/notes" $ do
    it "updates notes in the file" $
      withSystemTempDirectory "viewer-test" $ \tmpDir -> do
        let dir = tmpDir </> "applications"
        createDirectoryIfMissing True dir
        TIO.writeFile (dir </> "job.md") "# Test Job\n\nContent.\n"
        app <- mkApp dir "<html></html>"
        let req = SRequest (setPath defaultRequest { requestMethod = methodPost } "/api/file/job.md/notes") "My test notes"
        resp <- runSession (srequest req) app
        simpleStatus resp `shouldBe` status200
        content <- TIO.readFile (dir </> "job.md")
        content `shouldSatisfy` T.isInfixOf "## Notes"
        content `shouldSatisfy` T.isInfixOf "My test notes"

  describe "GET /api/stats" $ do
    it "returns stats for all directories" $
      withSystemTempDirectory "viewer-test" $ \tmpDir -> do
        let dir = tmpDir </> "applications"
        createDirectoryIfMissing True dir
        createDirectoryIfMissing True (tmpDir </> "done")
        createDirectoryIfMissing True (tmpDir </> "discarded")
        TIO.writeFile (dir </> "a.md") "# A"
        TIO.writeFile (dir </> "b.md") "# B"
        TIO.writeFile (tmpDir </> "done" </> "c.md") "# C"
        app <- mkApp dir "<html></html>"
        resp <- runSession (request (setPath defaultRequest "/api/stats")) app
        simpleStatus resp `shouldBe` status200
        let respBody = TE.decodeUtf8 (BL.toStrict (simpleBody resp))
        respBody `shouldSatisfy` T.isInfixOf "applications"
        respBody `shouldSatisfy` T.isInfixOf "done"
        respBody `shouldSatisfy` T.isInfixOf "discarded"
        respBody `shouldSatisfy` T.isInfixOf "queue"
