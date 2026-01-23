module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Viewer.Html (loadHtml)
import Viewer.Server (runServer)

defaultPort :: Int
defaultPort = 3000

defaultDir :: FilePath
defaultDir = "jobs/applications"

main :: IO ()
main = do
  args <- getArgs
  let port = fromMaybe defaultPort $ listToMaybe args >>= readMaybe
      dir  = fromMaybe defaultDir $ listToMaybe (drop 1 args)
  indexHtml <- loadHtml "."
  runServer port dir indexHtml
