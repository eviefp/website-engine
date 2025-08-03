module Main where

import Blog.Engine
import Blog.Prelude
import Blog.Settings (Settings (Settings))
import qualified Blog.Settings as Settings

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Development.Shake as Shake

-- This is just a sample generator. You should use the library and write your own.
main :: IO ()
main = do
  Shake.shakeArgs shakeOpts
    $ runReaderT run settings
 where
  settings :: Settings
  settings = Settings {source = "site", output = "docs"}

  shakeOpts :: Shake.ShakeOptions
  shakeOpts =
    Shake.shakeOptions
      { Shake.shakeLint = Just Shake.LintBasic
      , Shake.shakeTimings = False
      , Shake.shakeLintInside = [Settings.source settings]
      , Shake.shakeColor = True
      , Shake.shakeVerbosity = Shake.Verbose
      , Shake.shakeProgress = Shake.progressSimple
      }

run :: ReaderT Settings Shake.Rules ()
run = do
  itemsCache <- initItemsCache

  want [RelativePath "index.html"]

  "clean" ~> removeOutput

  "index.html" %> \path -> do
    need' ["css//*", "images//*"]

    posts <- itemsCache ["post//*.md"]
    putVerbose $ "found " <> show (length posts) <> " post(s)"

    pages <- itemsCache ["page//*.md"]
    putVerbose $ "found " <> show (length posts) <> " page(s)"

    wikis <- itemsCache ["wiki//*.md"]
    putVerbose $ "found " <> show (length posts) <> " wiki page(s)"

    need . fmap (RelativePath . (-<.> "html") . ("post/" </>) . T.unpack . id . fst) $ posts
    need . fmap (RelativePath . (-<.> "html") . ("page/" </>) . T.unpack . id . fst) $ pages
    need . fmap (RelativePath . (-<.> "html") . ("wiki/" </>) . T.unpack . id . fst) $ wikis

    let
      sortedPosts = fmap snd . sortOn (Down . publish . fst) $ posts
    writeFile (RelativePath "template/index.html") path
      . withMetadataObject "posts"
      . Aeson.toJSON
      $ sortedPosts

  -- static content
  "css//*" %> \path ->
    copyFile path path
  "images//*" %> \path ->
    copyFile path path

  -- posts
  "post//*.html" %> \path -> do
    need' ["post/content//*"]
    itemsCache ["post//*.md"] >>= generatePage "post" path (RelativePath "template/post.html")
  "post/content//*" %> \path ->
    copyFile path path

  -- pages
  "page//*.html" %> \path -> do
    need' ["page/content//*"]
    itemsCache ["page//*.md"] >>= generatePage "page" path (RelativePath "template/page.html")
  "page/content//*" %> \path ->
    copyFile path path

  -- wiki
  "wiki//*.html" %> \path -> do
    need' ["wiki/content//*"]
    itemsCache ["wiki//*.md"] >>= generatePage "wiki" path (RelativePath "template/wiki.html")
  "wiki/content//*" %> \path ->
    copyFile path path

  -- tags
  "tag/*.html" %> \path -> do
    allPosts <- itemsCache ["post//*.md"]
    allPages <- itemsCache ["page//*.md"]
    allWikis <- itemsCache ["wiki//*.md"]

    let
      tagName = T.pack . takeBaseName $ path
      posts = filter ((tagName `elem`) . tags . fst) allPosts
      pages = filter ((tagName `elem`) . tags . fst) allPages
      wikis = filter ((tagName `elem`) . tags . fst) allWikis

    writeFile (RelativePath "template/tag.html") path
      . addKey "posts" (Aeson.toJSON $ fmap snd posts)
      . addKey "pages" (Aeson.toJSON $ fmap snd pages)
      . addKey "wikis" (Aeson.toJSON $ fmap snd wikis)
      . withMetadataObject "tagName"
      . Aeson.toJSON
      $ tagName
