module Blog
  ( run
  ) where

import qualified Blog.Config as Config
import qualified Blog.Post as Post
import Blog.Prelude
import qualified Blog.Wiki as Wiki
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Development.Shake as Shake
import qualified Slick

buildIndex :: Shake.Action ()
buildIndex = do
  indexT <- Slick.compileTemplate' "site/template/index.html"
  let
    html = T.unpack $ Slick.substitute indexT (JSON.toJSON Config.metadata)
  Shake.writeFile' (Config.output </> "index.html") html

copyStaticFiles :: Shake.Action ()
copyStaticFiles = do
  paths <- Shake.getDirectoryFiles "./site/" ["css//*", "images//*"]
  void $ Shake.forP paths $ \path ->
    Shake.copyFileChanged ("site" </> path) (Config.output </> path)

run :: IO ()
run = Slick.slickWithOpts opts do
  Wiki.buildWiki *> Post.buildPost *> buildIndex *> copyStaticFiles
 where
  opts :: Shake.ShakeOptions
  opts =
    Shake.shakeOptions
      { Shake.shakeLint = Just Shake.LintBasic
      , Shake.shakeLintInside = ["./site/"]
      , Shake.shakeColor = True
      , Shake.shakeProgress = Shake.progressSimple
      }
