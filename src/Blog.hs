module Blog
  ( run
  , runNew
  ) where

import qualified Blog.Config as Config
import qualified Blog.Engine as Engine
import qualified Blog.Pages as Pages
import qualified Blog.Post as Post
import Blog.Prelude
import Blog.Settings (Settings)
import qualified Blog.Settings as Settings
import qualified Blog.Wiki as Wiki

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Development.Shake as Shake
import qualified Slick

buildIndex :: [Post.Post] -> Shake.Action ()
buildIndex posts = do
  indexT <- Slick.compileTemplate' "site/template/index.html"
  let
    sortedPosts = sortOn (Down . Post.publish) posts
  Shake.writeFile' (Config.output </> "index.html")
    . T.unpack
    . Slick.substitute indexT
    . Config.withMetadataObject "posts"
    . Aeson.toJSON
    $ sortedPosts

copyStaticFiles :: Shake.Action ()
copyStaticFiles = do
  paths <- Shake.getDirectoryFiles "./site/" ["css//*", "images//*"]
  postContents <- Shake.getDirectoryFiles "./site/" ["post/content//*"] -- post contents
  void $ Shake.forP (paths <> postContents) $ \path ->
    Shake.copyFileChanged ("site" </> path) (Config.output </> path)

run :: IO ()
run = Slick.slickWithOpts opts do
  copyStaticFiles
    *> Pages.buildPages
    *> Wiki.buildWiki
    *> Post.buildPosts
    >>= buildIndex
 where
  opts :: Shake.ShakeOptions
  opts =
    Shake.shakeOptions
      { Shake.shakeLint = Just Shake.LintBasic
      , Shake.shakeTimings = False
      , Shake.shakeLintInside = ["./site/"]
      , Shake.shakeColor = True
      , Shake.shakeVerbosity = Shake.Verbose
      , Shake.shakeProgress = Shake.progressSimple
      }

runNew :: IO ()
runNew = do
  settings <- Settings.parse
  let
    shakeOpts = mkShakeOpts settings
   in
    Shake.shakeArgs shakeOpts do
      Engine.run settings
 where
  mkShakeOpts :: Settings -> Shake.ShakeOptions
  mkShakeOpts opts =
    Shake.shakeOptions
      { Shake.shakeLint = Just Shake.LintBasic
      , Shake.shakeTimings = False
      , Shake.shakeLintInside = [Settings.source opts]
      , Shake.shakeColor = True
      , Shake.shakeVerbosity = Shake.Verbose
      , Shake.shakeProgress = Shake.progressSimple
      }
