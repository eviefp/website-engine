module Main where

import Blog
import qualified Blog.Path.Rel as RelPath
import Blog.Prelude

import qualified Path as P

-- This is just a sample generator. You should use the library and write your own.
main :: IO ()
main = do
  settings <- Settings <$> P.parseRelDir "site" <*> P.parseRelDir "docs" <*> pure Verbose
  runEngine settings run

run :: Rules ()
run = do
  want [[RelPath.outputFile|index.html|]]

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

    needItems posts
    needItems pages
    needItems wikis

    let
      sortedPosts = sortBy (Down . publish) posts
    writeFile [RelPath.sourceFile|template/index.html|] path
      . withMetadataObject "posts"
      . fmap metadata
      $ sortedPosts

  -- static content
  "css//*" %> \path ->
    copyFile (RelPath.asSource path) path
  "images//*" %> \path ->
    copyFile (RelPath.asSource path) path

  -- posts
  "post//*.html" %> \path -> do
    need' ["post/content//*"]
    sequence
      [ itemsCache post
      , itemsCache page
      , itemsCache wiki
      ]
      >>= generatePage "post" path [RelPath.sourceFile|template/post.html|]
  "post/content//*" %> \path ->
    copyFile (RelPath.asSource path) path

  -- pages
  "page//*.html" %> \path -> do
    need' ["page/content//*"]
    sequence
      [ itemsCache post
      , itemsCache page
      , itemsCache wiki
      ]
      >>= generatePage "page" path [RelPath.sourceFile|template/page.html|]
  "page/content//*" %> \path ->
    copyFile (RelPath.asSource path) path

  -- wiki
  "wiki//*.html" %> \path -> do
    need' ["wiki/content//*"]
    sequence
      [ itemsCache post
      , itemsCache page
      , itemsCache wiki
      ]
      >>= generatePage "wiki" path [RelPath.sourceFile|template/wiki.html|]
  "wiki/content//*" %> \path ->
    copyFile (RelPath.asSource path) path

  -- tags
  "tag/*.html" %> \path -> do
    allPosts <- itemsCache post
    allPages <- itemsCache page
    allWikis <- itemsCache wiki

    let
      tagName = tagNameFromPath path
      posts = filterByTags (tagName `elem`) allPosts
      pages = filterByTags (tagName `elem`) allPages
      wikis = filterByTags (tagName `elem`) allWikis

    writeFile [RelPath.sourceFile|template/tag.html|] path
      . addKey "posts" (fmap metadata posts)
      . addKey "pages" (fmap metadata pages)
      . addKey "wikis" (fmap metadata wikis)
      . withMetadataObject "tagName"
      $ tagName
