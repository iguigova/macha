{-# LANGUAGE OverloadedStrings #-}
module Viewer.MarkdownSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesFileExist)

import Viewer.Markdown (parseMetadata, listFiles, renderFile, moveFile, updateNotes, dirStats)
import Viewer.Types (FileInfo(..), DirStats(..))

sampleApplication :: T.Text
sampleApplication = T.unlines
  [ "# Acme Corp - Software Engineer"
  , ""
  , "**URL:** https://example.com/jobs/123"
  , "**Fit:** Strong fit"
  , ""
  , "## Job Description"
  , ""
  , "Build distributed systems."
  , ""
  , "## Fit Assessment"
  , ""
  , "**Alignment:**"
  , "- 10+ years backend experience"
  , ""
  , "**Gaps:**"
  , "- No Rust experience"
  , ""
  , "## Cover Letter"
  , ""
  , "Dear Hiring Team,"
  , ""
  , "I am writing to apply..."
  ]

spec :: Spec
spec = do
  describe "parseMetadata" $ do
    it "extracts title from first heading" $ do
      let info = parseMetadata "test.md" 0 sampleApplication
      fiTitle info `shouldBe` "Acme Corp - Software Engineer"

    it "extracts URL field" $ do
      let info = parseMetadata "test.md" 0 sampleApplication
      fiUrl info `shouldBe` "https://example.com/jobs/123"

    it "extracts fit rating" $ do
      let info = parseMetadata "test.md" 0 sampleApplication
      fiFit info `shouldBe` "Strong fit"

    it "preserves filename" $ do
      let info = parseMetadata "acme_corp.md" 0 sampleApplication
      fiFilename info `shouldBe` "acme_corp.md"

    it "uses filename as title when no heading exists" $ do
      let info = parseMetadata "fallback.md" 0 "No heading here\nJust text"
      fiTitle info `shouldBe` "fallback.md"

    it "returns empty string for missing URL" $ do
      let info = parseMetadata "test.md" 0 "# Title\n\n**Fit:** Good fit"
      fiUrl info `shouldBe` ""

    it "returns empty string for missing fit" $ do
      let info = parseMetadata "test.md" 0 "# Title\n\n**URL:** http://x.com"
      fiFit info `shouldBe` ""

    it "handles empty content" $ do
      let info = parseMetadata "empty.md" 0 ""
      fiTitle info `shouldBe` "empty.md"
      fiUrl info `shouldBe` ""
      fiFit info `shouldBe` ""

    it "takes first heading when multiple exist" $ do
      let content = "# First Title\n\n# Second Title\n**URL:** http://x.com"
      let info = parseMetadata "test.md" 0 content
      fiTitle info `shouldBe` "First Title"

    it "strips whitespace from extracted fields" $ do
      let content = "# Title  \n**URL:**   http://x.com   \n**Fit:**  Good fit  "
      let info = parseMetadata "test.md" 0 content
      fiUrl info `shouldBe` "http://x.com"
      fiFit info `shouldBe` "Good fit"

  describe "listFiles" $ do
    it "lists only .md files from directory" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        TIO.writeFile (dir </> "app1.md") sampleApplication
        TIO.writeFile (dir </> "app2.md") "# Second\n**Fit:** Good fit"
        writeFile (dir </> "notes.txt") "not a markdown file"
        files <- listFiles dir
        length files `shouldBe` 2

    it "parses metadata from each file" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        TIO.writeFile (dir </> "acme.md") sampleApplication
        files <- listFiles dir
        case files of
          [f] -> do
            fiTitle f `shouldBe` "Acme Corp - Software Engineer"
            fiFit f `shouldBe` "Strong fit"
          _ -> expectationFailure "Expected exactly one file"

    it "returns empty list for empty directory" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        files <- listFiles dir
        files `shouldBe` []

  describe "renderFile" $ do
    it "renders markdown to HTML" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        TIO.writeFile (dir </> "test.md") "# Hello\n\nA paragraph."
        html <- renderFile dir "test.md"
        html `shouldSatisfy` T.isInfixOf "<h1>"
        html `shouldSatisfy` T.isInfixOf "Hello"
        html `shouldSatisfy` T.isInfixOf "<p>"

    it "renders GFM tables" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        TIO.writeFile (dir </> "table.md") "| A | B |\n|---|---|\n| 1 | 2 |"
        html <- renderFile dir "table.md"
        html `shouldSatisfy` T.isInfixOf "<table>"

    it "auto-links URLs" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        TIO.writeFile (dir </> "link.md") "Visit https://example.com for info."
        html <- renderFile dir "link.md"
        html `shouldSatisfy` T.isInfixOf "<a href="

    it "renders strikethrough" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        TIO.writeFile (dir </> "strike.md") "This is ~~deleted~~ text."
        html <- renderFile dir "strike.md"
        html `shouldSatisfy` T.isInfixOf "<del>"

  describe "moveFile" $ do
    it "moves file from source to destination directory" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        let srcDir = dir </> "applications"
            destDir = dir </> "done"
        createDirectoryIfMissing True srcDir
        TIO.writeFile (srcDir </> "job.md") "# Test Job"
        moveFile srcDir "job.md" destDir
        srcExists <- doesFileExist (srcDir </> "job.md")
        destExists <- doesFileExist (destDir </> "job.md")
        srcExists `shouldBe` False
        destExists `shouldBe` True

    it "creates destination directory if it does not exist" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        let srcDir = dir </> "applications"
            destDir = dir </> "new-dir"
        createDirectoryIfMissing True srcDir
        TIO.writeFile (srcDir </> "job.md") "# Test"
        moveFile srcDir "job.md" destDir
        destExists <- doesFileExist (destDir </> "job.md")
        destExists `shouldBe` True

    it "preserves file content after move" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        let srcDir = dir </> "src"
            destDir = dir </> "dest"
        createDirectoryIfMissing True srcDir
        let content = "# Job\n\n**Fit:** Strong fit\n"
        TIO.writeFile (srcDir </> "job.md") content
        moveFile srcDir "job.md" destDir
        result <- TIO.readFile (destDir </> "job.md")
        result `shouldBe` content

  describe "updateNotes" $ do
    it "appends notes section to file without existing notes" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        TIO.writeFile (dir </> "job.md") "# Title\n\nSome content.\n"
        updateNotes dir "job.md" "My notes here"
        result <- TIO.readFile (dir </> "job.md")
        result `shouldSatisfy` T.isInfixOf "## Notes"
        result `shouldSatisfy` T.isInfixOf "My notes here"

    it "replaces existing notes section" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        TIO.writeFile (dir </> "job.md") "# Title\n\nContent.\n\n## Notes\n\nOld notes\n"
        updateNotes dir "job.md" "New notes"
        result <- TIO.readFile (dir </> "job.md")
        result `shouldSatisfy` T.isInfixOf "New notes"
        result `shouldSatisfy` (not . T.isInfixOf "Old notes")

    it "preserves content before notes section" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        TIO.writeFile (dir </> "job.md") "# Title\n\n**Fit:** Good fit\n\n## Notes\n\nOld\n"
        updateNotes dir "job.md" "Updated"
        result <- TIO.readFile (dir </> "job.md")
        result `shouldSatisfy` T.isInfixOf "# Title"
        result `shouldSatisfy` T.isInfixOf "**Fit:** Good fit"

    it "handles empty notes" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        TIO.writeFile (dir </> "job.md") "# Title\n"
        updateNotes dir "job.md" ""
        result <- TIO.readFile (dir </> "job.md")
        result `shouldSatisfy` T.isInfixOf "## Notes"

  describe "dirStats" $ do
    it "counts markdown files in directory" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        TIO.writeFile (dir </> "a.md") "# A"
        TIO.writeFile (dir </> "b.md") "# B"
        TIO.writeFile (dir </> "c.txt") "not markdown"
        stats <- dirStats dir
        dsTotal stats `shouldBe` 2

    it "returns zeros for nonexistent directory" $ do
      stats <- dirStats "/tmp/nonexistent-dir-viewer-test-xyz"
      stats `shouldBe` DirStats 0 0 0 0

    it "returns zeros for empty directory" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        stats <- dirStats dir
        stats `shouldBe` DirStats 0 0 0 0

    it "counts recent files in today/week/month" $
      withSystemTempDirectory "viewer-test" $ \dir -> do
        TIO.writeFile (dir </> "recent.md") "# Recent"
        stats <- dirStats dir
        dsTotal stats `shouldBe` 1
        dsToday stats `shouldBe` 1
        dsWeek stats `shouldBe` 1
        dsMonth stats `shouldBe` 1
