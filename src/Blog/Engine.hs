module Blog.Engine
  ( runEngine
  , want
  , copyFile
  , generatePage
  , initItemsCache
  , writeFile
  , putVerbose
  , putInfo
  , putWarn
  , putError
  , removeOutput
  , (%>)
  , need
  , need'
  , needItems
  , (~>)
  , withMetadataObject
  , addKey
  , takeBaseName
  , sortBy
  , filterByTags
  )
where

import qualified Blog.Content as Content
import Blog.Item
import Blog.Path.Rel as Rel
import Blog.Prelude
import qualified Blog.Settings as Settings
import Blog.Types

import qualified Chronos
import Control.Exception.Extra (Partial)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as T
import qualified Development.Shake as Shake
import qualified Development.Shake.Plus as SP
import qualified System.FilePath as U

runEngine :: Shake.ShakeOptions -> Settings.Settings -> Rules () -> IO ()
runEngine args settings rules =
  Shake.shakeArgs args
    $ SP.runShakePlus settings rules

-- | Will attempt to find the appropriate Rules that match the requested paths.
--
-- You can define rules using @'(%>)'@.
--
-- 't' can be list, Maybe, etc.
want :: (Partial, Traversable t) => t (Path Rel.Output File) -> Rules ()
want files = traverse outputToRel files >>= SP.wantP

-- | Copy a file from a source-relative path to a destination-relative path.
copyFile :: (Partial) => Path Rel.Source File -> Path Rel.Output File -> Action ()
copyFile src' dest' = do
  src <- sourceToRel src'
  dest <- outputToRel dest'
  putInfo $ "[copyFile] " <> show src <> " -> " <> show dest
  quietly $ SP.copyFileChanged src dest

-- | Generates a html page. The arguments are:
--
-- * name: the key for the cache for the current item
-- * path: the destination-relative path for the page
--   (its baseName is assuemd to be the itemId)
-- * templatePath: the source-relative path to the template
-- * cache: a lookup dictionary with all the content items of the website;
--   one key must match the name, and under this key there has to be an item
--   with its itemId equal to the baseName of the path
--
-- The mustache template will get all metadata fields from the markdown file
-- under a key that matches the passed in 'name' argument.
-- It will also get an extra field labeled 'content', which is the HTML generated
-- content.
--
-- See 'Item' for more details.
generatePage
  :: (Partial)
  => ItemKind
  -> Path Rel.Output File
  -> Path Rel.Source File
  -> [(ItemKind, [Item])]
  -> Action ()
generatePage name path templatePath cache = do
  items <- case lookup name cache of
    Nothing -> putError $ "[generatePage] Cannot find item in cache: " <> show name
    Just i -> pure i

  case find ((== (ItemId . T.pack . takeBaseName $ path)) . id) items of
    Nothing ->
      putError
        . join
        $ [ "["
          , show name
          , "] Internal error: could not find "
          , show name
          , " :"
          , show path
          ]
    Just item -> do
      need
        . fmap ([outputDir|tag|] </>)
        <=< traverse (addExtension ".html")
        <=< traverse parseRelFile
        $ (T.unpack . getTagName <$> tags item)

      putVerbose $ "[generatePage] Generating content for " <> show path
      content <- Content.genenerateHtmlWithFixedWikiLinks cache name . documentContent $ item
      putVerbose $ "[generatePage] Generated content:\n" <> show content

      putInfo $ "[generatePage] Generated " <> show path
      writeFile templatePath path
        . withMetadataObject (T.unpack . getItemKind $ name)
        . addKey "content" content
        . Aeson.toJSON
        . metadata
        $ item

-- | Initialise the items cache. This function takes no arguments.
-- Returns a function that takes a tuple of a item-group key and a list of file patterns.
-- Example:
--
-- @
--     itemsCache <- initItemsCache
--     -- ...
--     "post/*.html" %> \path -> do
--       posts <- initItemsCache ("post", ["post/*.md"])
-- @
--
-- The value of posts will be @("post", [...])@.
-- See 'Item' for more details.
initItemsCache :: Rules ((ItemKind, [Shake.FilePattern]) -> Action (ItemKind, [Item]))
initItemsCache = do
  SP.newCache \(k, path) -> do
    source <- asks Settings.source
    postsPaths <- SP.getDirectoryFiles source path
    today <- liftIO Chronos.today
    items <-
      SP.forP
        postsPaths
        ( \postPath ->
            fmap (mkItem today postPath)
              . ( \(meta, blocks) -> do
                    putVerbose $ "Metadata: \n" <> show meta
                    putVerbose $ "Content: \n" <> show blocks
                    pure (meta, blocks)
                )
              <=< Content.markdownToMetaAndContent
              <=< SP.readFileIn' source
              $ postPath
        )
    traverse_ (putInfo . show) . lefts $ items
    pure . (k,) . rights $ items

-- | Perform mustache substitution and write the new file. Arguments:
--
-- * templatePath: source-relative path for the template
-- * path: destination-relative path for the output file
-- * content: the aeson values to pass as inputs to mustache for substitution
writeFile
  :: (Aeson.ToJSON a)
  => Path Rel.Source File
  -> Path Rel.Output File
  -> a
  -> Action ()
writeFile templatePath path' content = do
  path <- outputToRel path'
  template <- Content.compileTemplate templatePath
  putInfo
    $ "[writeFile] Writing "
    <> show path
    <> " with template "
    <> show templatePath
  SP.writeFile' path
    . Content.substitute template
    $ content

putVerbose :: String -> Action ()
putVerbose = SP.liftAction . Shake.putVerbose

putInfo :: String -> Action ()
putInfo = SP.liftAction . Shake.putInfo

putWarn :: String -> Action ()
putWarn = SP.liftAction . Shake.putWarn

-- | Logs an error message and also exits the program.
putError :: (Partial) => String -> Action a
putError err = do
  SP.liftAction $ Shake.putError err
  crashWith err

quietly :: Action a -> Action a
quietly act = SP.withRunInAction (\run -> run act)

-- | Nukes the output directory.
removeOutput :: Action ()
removeOutput = do
  output <- asks Settings.output
  putInfo $ "[removeOutput] Nuking output " <> show output
  SP.removeFiles output ["//"]

-- | Defines the build rules for an output path.
-- Example:
--
-- @
--     "post/*.html" %> \path -> do
--       ...
--       -- must generate path by the end, otherwise an error will be raised
-- @
(%>) :: Shake.FilePattern -> (Path Rel.Output File -> Action ()) -> Rules ()
(%>) pat action = do
  output <- asks Settings.output
  (toFilePath output U.</> pat) SP.%> \path -> do
    putInfo $ "[%>] Generating " <> show path
    p <- stripProperPrefix output path
    action (asOutput p)

-- | Declare dependencies between rules through paths.
need :: (Traversable t) => t (Path Rel.Output File) -> Action ()
need path = traverse outputToRel path >>= SP.needP

-- | Declare dependencies between rules through paths.
need' :: [Shake.FilePattern] -> Action ()
need' pat = do
  source <- asks Settings.source
  SP.getDirectoryFiles source pat >>= need . fmap asOutput

-- | Needs all items in a cache.
-- Assumes that the output path is @<ItemKind>/<itemId.html>@
needItems :: (ItemKind, [Item]) -> Action ()
needItems (itemKind, items) = do
  itemDir <- fmap asOutput . parseRelDir . T.unpack . getItemKind $ itemKind
  need
    . fmap (itemDir </>)
    <=< traverse (addExtension ".html")
    <=< traverse parseRelFile
    . fmap (T.unpack . getItemId . id)
    $ items

-- | Declare a pseudo-rule using a name rather than a path.
-- This is required because @'(%>)'@ would crash the program if used with a fake name.
-- Can be used to create rules for cleaning or other auxiliary tasks.
(~>) :: String -> Action () -> Rules ()
(~>) = SP.phony

-- | Given some key @"k"@ and JSON value @v@, creates a JSON object @{ "k": v }@.
withMetadataObject :: (Aeson.ToJSON a) => String -> a -> Aeson.Value
withMetadataObject name = Aeson.Object . KeyMap.singleton (Key.fromString name) . Aeson.toJSON

-- | Given a key @"k"@, JSON value @v@, and an existing object @o@,
-- this function adds the key @"k": v@ to @o@.
addKey :: (Aeson.ToJSON a) => String -> a -> Aeson.Value -> Aeson.Value
addKey k v object =
  case object of
    Aeson.Object o -> Aeson.Object . KeyMap.insert (Key.fromString k) (Aeson.toJSON v) $ o
    other -> crashWith $ "addMetadataObject: json is not an object, found: " <> show other

-- | A specialised version of `sortOn` that makes it easier to use with @(ItemKind, [Item])@.
-- Use as @sortBy (Down . publish) posts@ to get newest posts first.
sortBy :: (Ord a) => (Item -> a) -> (ItemKind, [Item]) -> [Item]
sortBy f = sortOn f . snd

-- | A specialised version for `filter` that makes it easier to use with @(ItemKind, [Item])@.
-- Use as @filterByTags (myTag `elem`) posts@.
filterByTags :: ([TagName] -> Bool) -> (ItemKind, [Item]) -> [Item]
filterByTags f = filter (f . tags) . snd
