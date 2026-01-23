module Viewer.Html (loadHtml) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))

loadHtml :: FilePath -> IO Text
loadHtml baseDir = TIO.readFile (baseDir </> "static" </> "index.html")
