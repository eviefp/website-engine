module Main where

import Blog.Engine
import Blog.Item
import Blog.Prelude
import Blog.Settings (Settings (Settings))
import qualified Blog.Settings as Settings

import Blog.Path
import Blog.Types (Rules)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Development.Shake as Shake
import qualified Development.Shake.Plus as SP
import qualified Path as P

-- This is just a sample generator. You should use the library and write your own.
main :: IO ()
main = do
  source <- P.parseRelDir "site"
  output <- P.parseRelDir "docs"
  Shake.shakeArgs (shakeOpts Settings {..})
    $ SP.runShakePlus Settings {..} run
 where
  shakeOpts :: Settings -> Shake.ShakeOptions
  shakeOpts s =
    Shake.shakeOptions
      { Shake.shakeLint = Just Shake.LintBasic
      , Shake.shakeTimings = False
      , Shake.shakeLintInside = P.toFilePath <$> [Settings.source s]
      , Shake.shakeColor = True
      , Shake.shakeVerbosity = Shake.Verbose
      , Shake.shakeProgress = Shake.progressSimple
      }

run :: Rules ()
run = do
  want [asOutputRel [relfile|"index.html"|]]

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

    need
      . fmap (asOutputRel . ([P.reldir|"post"|] </>))
      <=< traverse (P.addExtension "html")
      <=< traverse P.parseRelFile
      . fmap (T.unpack . getItemId . id)
      . snd
      $ posts
    need
      . fmap (asOutputRel . ([P.reldir|"page"|] </>))
      <=< traverse (P.addExtension "html")
      <=< traverse P.parseRelFile
      . fmap (T.unpack . getItemId . id)
      . snd
      $ pages
    need
      . fmap (asOutputRel . ([P.reldir|"wiki"|] </>))
      <=< traverse (P.addExtension "html")
      <=< traverse P.parseRelFile
      . fmap (T.unpack . getItemId . id)
      . snd
      $ wikis

    let
      sortedPosts = sortOn (Down . publish) $ snd posts
    writeFile (asSourceRel [relfile|"template/index.html"|]) path
      . withMetadataObject "posts"
      . Aeson.toJSON
      . fmap metadata
      $ sortedPosts

  -- static content
  "css//*" %> \path ->
    copyFile (asSourceRel path) path
  "images//*" %> \path ->
    copyFile (asSourceRel path) path

  -- posts
  "post//*.html" %> \path -> do
    need' ["post/content//*"]
    sequence
      [ itemsCache post
      , itemsCache page
      , itemsCache wiki
      ]
      >>= generatePage "post" path (asSourceRel [relfile|"template/post.html"|])
  "post/content//*" %> \path ->
    copyFile (asSourceRel path) path

  -- pages
  "page//*.html" %> \path -> do
    need' ["page/content//*"]
    sequence
      [ itemsCache post
      , itemsCache page
      , itemsCache wiki
      ]
      >>= generatePage "page" path (asSourceRel [relfile|"template/page.html"|])
  "page/content//*" %> \path ->
    copyFile (asSourceRel path) path

  -- wiki
  "wiki//*.html" %> \path -> do
    need' ["wiki/content//*"]
    sequence
      [ itemsCache post
      , itemsCache page
      , itemsCache wiki
      ]
      >>= generatePage "wiki" path (asSourceRel [relfile|"template/wiki.html"|])
  "wiki/content//*" %> \path ->
    copyFile (asSourceRel path) path

  -- tags
  "tag/*.html" %> \path -> do
    allPosts <- itemsCache post
    allPages <- itemsCache page
    allWikis <- itemsCache wiki

    let
      tagName = TagName . T.pack . takeBaseName $ path
      posts = filter ((tagName `elem`) . tags) . snd $ allPosts
      pages = filter ((tagName `elem`) . tags) . snd $ allPages
      wikis = filter ((tagName `elem`) . tags) . snd $ allWikis

    writeFile (asSourceRel [relfile|"template/tag.html"|]) path
      . addKey "posts" (Aeson.toJSON $ fmap metadata posts)
      . addKey "pages" (Aeson.toJSON $ fmap metadata pages)
      . addKey "wikis" (Aeson.toJSON $ fmap metadata wikis)
      . withMetadataObject "tagName"
      . Aeson.toJSON
      . getTagName
      $ tagName
