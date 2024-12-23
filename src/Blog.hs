module Blog
  ( run
  ) where

import qualified Blog.Config as Config
import Blog.Prelude
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Development.Shake as Shake
import Development.Shake.FilePath ((-<.>))
import qualified Development.Shake.FilePath as Shake
import qualified Slick

output :: FilePath
output = "generated"

buildIndex :: Shake.Action ()
buildIndex = do
  indexT <- Slick.compileTemplate' "site/template/index.html"
  let
    html = T.unpack $ Slick.substitute indexT (JSON.toJSON Config.metadata)
  Shake.writeFile' (output </> "index.html") html

copyStaticFiles :: Shake.Action ()
copyStaticFiles = do
  paths <- Shake.getDirectoryFiles "./site/" ["css//*", "images//*"]
  void $ Shake.forP paths $ \path ->
    Shake.copyFileChanged ("site" </> path) (output </> path)

buildWiki :: Shake.Action ()
buildWiki = void $ flip Shake.forP buildWikiPage =<< Shake.getDirectoryFiles "." ["site/wiki//*.md"]

buildWikiPage :: FilePath -> Shake.Action ()
buildWikiPage path = do
  post <- Slick.markdownToHTML . T.pack =<< Shake.readFile' path
  let
    url = T.pack . Shake.dropDirectory1 $ path -<.> "html"
  template <- Slick.compileTemplate' "site/template/wiki.html"
  Shake.writeFile' (output </> T.unpack url)
    . T.unpack
    . Slick.substitute template
    . Config.withMetadataObject "wiki"
    $ post

run :: IO ()
run = Slick.slickWithOpts opts do
  buildWiki *> buildIndex *> copyStaticFiles
 where
  opts :: Shake.ShakeOptions
  opts =
    Shake.shakeOptions
      { Shake.shakeLint = Just Shake.LintBasic
      , Shake.shakeLintInside = ["./site/"]
      , Shake.shakeColor = True
      , Shake.shakeProgress = Shake.progressSimple
      }
