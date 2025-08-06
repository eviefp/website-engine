module Main where

import Blog.Engine
import Blog.Item
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
  want [RelativePath "index.html"]

  let
    post = ("post", ["post//*.md"])
    page = ("page", ["page//*.md"])
    wiki = ("wiki", ["wiki//*.md"])
  itemsCache <- initItemsCache

  "clean" ~> removeOutput

  "index.html" %> \path -> do
    need' ["css//*", "images//*"]

    posts <- itemsCache post
    pages <- itemsCache page
    wikis <- itemsCache wiki

    need . fmap (RelativePath . ("post" </>) . (-<.> "html") . T.unpack . id) . snd $ posts
    need . fmap (RelativePath . ("page" </>) . (-<.> "html") . T.unpack . id) . snd $ pages
    need . fmap (RelativePath . ("wiki" </>) . (-<.> "html") . T.unpack . id) . snd $ wikis

    let
      sortedPosts = sortOn (Down . publish) $ snd posts
    writeFile (RelativePath "template/index.html") path
      . withMetadataObject "posts"
      . Aeson.toJSON
      . fmap metadata
      $ sortedPosts

  -- static content
  "css//*" %> \path ->
    copyFile path path
  "images//*" %> \path ->
    copyFile path path

  -- posts
  "post//*.html" %> \path -> do
    need' ["post/content//*"]
    sequence
      [ itemsCache post
      , itemsCache page
      , itemsCache wiki
      ]
      >>= generatePage "post" path (RelativePath "template/post.html")
  "post/content//*" %> \path ->
    copyFile path path

  -- pages
  "page//*.html" %> \path -> do
    need' ["page/content//*"]
    sequence
      [ itemsCache post
      , itemsCache page
      , itemsCache wiki
      ]
      >>= generatePage "page" path (RelativePath "template/page.html")
  "page/content//*" %> \path ->
    copyFile path path

  -- wiki
  "wiki//*.html" %> \path -> do
    need' ["wiki/content//*"]
    sequence
      [ itemsCache post
      , itemsCache page
      , itemsCache wiki
      ]
      >>= generatePage "wiki" path (RelativePath "template/wiki.html")
  "wiki/content//*" %> \path ->
    copyFile path path

  -- tags
  "tag/*.html" %> \path -> do
    allPosts <- itemsCache post
    allPages <- itemsCache page
    allWikis <- itemsCache wiki

    let
      tagName = T.pack . takeBaseName $ path
      posts = filter ((tagName `elem`) . tags) . snd $ allPosts
      pages = filter ((tagName `elem`) . tags) . snd $ allPages
      wikis = filter ((tagName `elem`) . tags) . snd $ allWikis

    writeFile (RelativePath "template/tag.html") path
      . addKey "posts" (Aeson.toJSON $ fmap metadata posts)
      . addKey "pages" (Aeson.toJSON $ fmap metadata pages)
      . addKey "wikis" (Aeson.toJSON $ fmap metadata wikis)
      . withMetadataObject "tagName"
      . Aeson.toJSON
      $ tagName
