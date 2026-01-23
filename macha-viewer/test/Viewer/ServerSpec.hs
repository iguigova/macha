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

  get "/api/:dir/files" $ do
    dirParam <- captureParam "dir" :: ActionM Text
    let targetDir = parentDir </> T.unpack dirParam
    files <- liftIO $ listFiles targetDir
    json files

  get "/api/:dir/file/:name" $ do
    dirParam <- captureParam "dir" :: ActionM Text
    name <- captureParam "name" :: ActionM Text
    let targetDir = parentDir </> T.unpack dirParam
    rendered <- liftIO $ renderFile targetDir name
    setHeader "Content-Type" "text/html; charset=utf-8"
    raw . LTE.encodeUtf8 . LT.fromStrict $ rendered

  post "/api/:dir/file/:name/apply" $ do
    dirParam <- captureParam "dir" :: ActionM Text
    name <- captureParam "name" :: ActionM Text
    let targetDir = parentDir </> T.unpack dirParam
    liftIO $ moveFile targetDir name (parentDir </> "done")
    json $ object ["status" .= ("ok" :: Text)]

  post "/api/:dir/file/:name/discard" $ do
    dirParam <- captureParam "dir" :: ActionM Text
    name <- captureParam "name" :: ActionM Text
    let targetDir = parentDir </> T.unpack dirParam
    liftIO $ moveFile targetDir name (parentDir </> "discarded")
    json $ object ["status" .= ("ok" :: Text)]

  post "/api/:dir/file/:name/notes" $ do
    dirParam <- captureParam "dir" :: ActionM Text
    name <- captureParam "name" :: ActionM Text
    reqBody <- body
    let targetDir = parentDir </> T.unpack dirParam
        notes = TE.decodeUtf8 (BL.toStrict reqBody)
    liftIO $ updateNotes targetDir name (T.strip notes)
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

  describe "GET /api/:dir/files" $ do
    it "returns JSON array with file info" $
      withSystemTempDirectory "viewer-test" $ \tmpDir -> do
        let dir = tmpDir </> "applications"
        createDirectoryIfMissing True dir
        TIO.writeFile (dir </> "test.md") "# Test Co - Dev\n**Fit:** Good fit\n**URL:** http://x.com"
        app <- mkApp dir "<html></html>"
        resp <- runSession (request (setPath defaultRequest "/api/applications/files")) app
        simpleStatus resp `shouldBe` status200
        let respBody = TE.decodeUtf8 (BL.toStrict (simpleBody resp))
        respBody `shouldSatisfy` T.isInfixOf "Test Co - Dev"
        respBody `shouldSatisfy` T.isInfixOf "Good fit"

    it "returns empty array for empty directory" $
      withSystemTempDirectory "viewer-test" $ \tmpDir -> do
        let dir = tmpDir </> "applications"
        createDirectoryIfMissing True dir
        app <- mkApp dir "<html></html>"
        resp <- runSession (request (setPath defaultRequest "/api/applications/files")) app
        BL.toStrict (simpleBody resp) `shouldBe` "[]"

    it "lists files from different directories" $
      withSystemTempDirectory "viewer-test" $ \tmpDir -> do
        let dir = tmpDir </> "applications"
            doneDir' = tmpDir </> "done"
        createDirectoryIfMissing True dir
        createDirectoryIfMissing True doneDir'
        TIO.writeFile (dir </> "app.md") "# App Job"
        TIO.writeFile (doneDir' </> "done.md") "# Done Job"
        app <- mkApp dir "<html></html>"
        respApps <- runSession (request (setPath defaultRequest "/api/applications/files")) app
        respDone <- runSession (request (setPath defaultRequest "/api/done/files")) app
        let appsBody = TE.decodeUtf8 (BL.toStrict (simpleBody respApps))
            doneBody = TE.decodeUtf8 (BL.toStrict (simpleBody respDone))
        appsBody `shouldSatisfy` T.isInfixOf "App Job"
        appsBody `shouldSatisfy` (not . T.isInfixOf "Done Job")
        doneBody `shouldSatisfy` T.isInfixOf "Done Job"
        doneBody `shouldSatisfy` (not . T.isInfixOf "App Job")

  describe "GET /api/:dir/file/:name" $ do
    it "renders markdown to HTML" $
      withSystemTempDirectory "viewer-test" $ \tmpDir -> do
        let dir = tmpDir </> "applications"
        createDirectoryIfMissing True dir
        TIO.writeFile (dir </> "job.md") "# Hello World\n\nSome text."
        app <- mkApp dir "<html></html>"
        resp <- runSession (request (setPath defaultRequest "/api/applications/file/job.md")) app
        simpleStatus resp `shouldBe` status200
        let respBody = TE.decodeUtf8 (BL.toStrict (simpleBody resp))
        respBody `shouldSatisfy` T.isInfixOf "<h1>"
        respBody `shouldSatisfy` T.isInfixOf "Hello World"

  describe "POST /api/:dir/file/:name/apply" $ do
    it "moves file to done directory" $
      withSystemTempDirectory "viewer-test" $ \tmpDir -> do
        let dir = tmpDir </> "applications"
        createDirectoryIfMissing True dir
        TIO.writeFile (dir </> "job.md") "# Test Job"
        app <- mkApp dir "<html></html>"
        let req = SRequest (setPath defaultRequest { requestMethod = methodPost } "/api/applications/file/job.md/apply") ""
        resp <- runSession (srequest req) app
        simpleStatus resp `shouldBe` status200
        srcExists <- doesFileExist (dir </> "job.md")
        destExists <- doesFileExist (tmpDir </> "done" </> "job.md")
        srcExists `shouldBe` False
        destExists `shouldBe` True

  describe "POST /api/:dir/file/:name/discard" $ do
    it "moves file to discarded directory" $
      withSystemTempDirectory "viewer-test" $ \tmpDir -> do
        let dir = tmpDir </> "applications"
        createDirectoryIfMissing True dir
        TIO.writeFile (dir </> "job.md") "# Test Job"
        app <- mkApp dir "<html></html>"
        let req = SRequest (setPath defaultRequest { requestMethod = methodPost } "/api/applications/file/job.md/discard") ""
        resp <- runSession (srequest req) app
        simpleStatus resp `shouldBe` status200
        srcExists <- doesFileExist (dir </> "job.md")
        destExists <- doesFileExist (tmpDir </> "discarded" </> "job.md")
        srcExists `shouldBe` False
        destExists `shouldBe` True

  describe "POST /api/:dir/file/:name/notes" $ do
    it "updates notes in the file" $
      withSystemTempDirectory "viewer-test" $ \tmpDir -> do
        let dir = tmpDir </> "applications"
        createDirectoryIfMissing True dir
        TIO.writeFile (dir </> "job.md") "# Test Job\n\nContent.\n"
        app <- mkApp dir "<html></html>"
        let req = SRequest (setPath defaultRequest { requestMethod = methodPost } "/api/applications/file/job.md/notes") "My test notes"
        resp <- runSession (srequest req) app
        simpleStatus resp `shouldBe` status200
        fileContent <- TIO.readFile (dir </> "job.md")
        fileContent `shouldSatisfy` T.isInfixOf "## Notes"
        fileContent `shouldSatisfy` T.isInfixOf "My test notes"

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
