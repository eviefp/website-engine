module Blog.Engine
  ( want
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
  , (~>)
  , withMetadataObject
  , addKey
  , takeBaseName
  , options
  , markdownToMetaAndContent
  )
where

import Blog.Item
import Blog.Path
import Blog.Prelude
import qualified Blog.Settings as Settings
import Blog.Types
import qualified Blog.Wikilinks as CL

import qualified Chronos
import Control.Exception.Extra (Partial)
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Functor.Identity (runIdentity)
import qualified Data.Text as T
import qualified Development.Shake as Shake
import qualified Development.Shake.FilePath as Shake
import qualified Development.Shake.Plus as SP
import qualified Slick
import qualified Slick.Pandoc as Slick
import qualified System.FilePath as U
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as PW

-- | Will attempt to find the appropriate Rules that match the requested paths.
--
-- You can define rules using @'(%>)'@.
--
-- 't' can be list, Maybe, etc.
want :: (Partial, Traversable t) => t (Path OutputRel File) -> Rules ()
want files = traverse outputToRel files >>= SP.wantP

-- | Copy a file from a source-relative path to a destination-relative path.
copyFile :: (Partial) => Path SourceRel File -> Path OutputRel File -> Action ()
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
  -> Path OutputRel File
  -> Path SourceRel File
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
        . fmap ([outputRelDir|"tag"|] </>)
        <=< traverse (addExtension ".html")
        <=< traverse parseRelFile
        $ (T.unpack . getTagName <$> tags item)

      putVerbose $ "[generatePage] Generating content for " <> show path
      content <- genenerateHtmlWithFixedWikiLinks cache name . documentContent $ item
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
              <=< markdownToMetaAndContent
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
  :: Path SourceRel File
  -> Path OutputRel File
  -> Aeson.Value
  -> Action ()
writeFile templatePath' path' content = do
  templatePath <- sourceToRel templatePath'
  path <- outputToRel path'
  template <- SP.liftAction . Slick.compileTemplate' . toFilePath $ templatePath
  putInfo
    $ "[writeFile] Writing "
    <> show path
    <> " with template "
    <> show templatePath
  SP.writeFile' path
    . Slick.substitute template
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
(%>) :: Shake.FilePattern -> (Path OutputRel File -> Action ()) -> Rules ()
(%>) pat action = do
  output <- asks Settings.output
  (toFilePath output U.</> pat) SP.%> \path -> do
    putInfo $ "[%>] Generating " <> show path
    p <- stripProperPrefix output path
    action (asOutputRel p)

-- | Declare dependencies between rules through paths.
need :: (Traversable t) => t (Path OutputRel File) -> Action ()
need path = traverse outputToRel path >>= SP.needP

-- | Declare dependencies between rules through paths.
need' :: [Shake.FilePattern] -> Action ()
need' pat = do
  source <- asks Settings.source
  SP.getDirectoryFiles source pat >>= need . fmap asOutputRel

-- | Declare a pseudo-rule using a name rather than a path.
-- This is required because @'(%>)'@ would crash the program if used with a fake name.
-- Can be used to create rules for cleaning or other auxiliary tasks.
(~>) :: String -> Action () -> Rules ()
(~>) = SP.phony

-- | Given some key @"k"@ and JSON value @v@, creates a JSON object @{ "k": v }@.
withMetadataObject :: String -> Aeson.Value -> Aeson.Value
withMetadataObject name json = Aeson.Object $ KeyMap.singleton (Key.fromString name) json

-- | Given a key @"k"@, JSON value @v@, and an existing object @o@,
-- this function adds the key @"k": v@ to @o@.
addKey :: String -> Aeson.Value -> Aeson.Value -> Aeson.Value
addKey k v object =
  case object of
    Aeson.Object o -> Aeson.Object . KeyMap.insert (Key.fromString k) v $ o
    other -> crashWith $ "addMetadataObject: json is not an object, found: " <> show other

-- | Given a relative path, returns the base name.
-- The base name is the name of the file (so it strips any directories and file extension).
takeBaseName :: Path b File -> String
takeBaseName = Shake.takeBaseName . toFilePath

-- | Default Pandoc markdown reader options. Using other options may break some of the helpers.
options :: Pandoc.ReaderOptions
options =
  Slick.defaultMarkdownOptions
    { Pandoc.readerExtensions =
        mconcat
          [ Pandoc.extensionsFromList
              [ Pandoc.Ext_auto_identifiers -- todo: test
              , Pandoc.Ext_fenced_code_attributes -- todo: test
              , Pandoc.Ext_footnotes -- todo: test
              , Pandoc.Ext_wikilinks_title_after_pipe -- needed for obsidian links
              , Pandoc.Ext_yaml_metadata_block -- needed for metadata
              ]
          , Pandoc.githubMarkdownExtensions
          ]
    }

-- | Exported for testing. Should not be used.
markdownToMetaAndContent :: (MonadIO m) => Text -> m (Aeson.Value, [Pandoc.Block])
markdownToMetaAndContent content = do
  let
    writer = Pandoc.writeHtml5String Slick.defaultHtml5Options
  (Pandoc.Pandoc meta' blocks) <- unPandocM $ Pandoc.readMarkdown options content
  meta <- flattenMeta writer meta'
  pure (meta, blocks)

flattenMeta
  :: (MonadIO m)
  => (Pandoc.Pandoc -> Pandoc.PandocIO T.Text)
  -> Pandoc.Meta
  -> m Aeson.Value
flattenMeta writer (Pandoc.Meta meta) = Aeson.toJSON <$> traverse go meta
 where
  go :: (MonadIO m) => Pandoc.MetaValue -> m Aeson.Value
  go (Pandoc.MetaMap m) = Aeson.toJSON <$> traverse go m
  go (Pandoc.MetaList m) = Aeson.toJSONList <$> traverse go m
  go (Pandoc.MetaBool m) = pure $ Aeson.toJSON m
  go (Pandoc.MetaString m) = pure $ Aeson.toJSON m
  go (Pandoc.MetaInlines m) = Aeson.toJSON <$> (unPandocM . writer . Pandoc.Pandoc mempty . (: []) . Pandoc.Plain $ m)
  go (Pandoc.MetaBlocks m) = Aeson.toJSON <$> (unPandocM . writer . Pandoc.Pandoc mempty $ m)

genenerateHtmlWithFixedWikiLinks
  :: (Partial, Traversable t)
  => [(ItemKind, [Item])]
  -> ItemKind
  -> t Pandoc.Block
  -> Action Aeson.Value
genenerateHtmlWithFixedWikiLinks cache def blocks = do
  let
    (result, logs) =
      runIdentity
        . flip State.runStateT []
        . Except.runExceptT
        $ PW.walkM (CL.transform cache def) (toList blocks)
  traverse_ printLog logs
  case result of
    Left _ -> crashWith "[Wikilinks] unrecoverable error. Stopping."
    Right fixedBlocks -> do
      let
        writer = Pandoc.writeHtml5String Slick.defaultHtml5Options
        doc = Pandoc.Pandoc mempty $ toList fixedBlocks
      outText <- unPandocM $ writer doc
      pure $ Aeson.String outText

unPandocM :: (MonadIO m) => Pandoc.PandocIO a -> m a
unPandocM p = do
  result <- liftIO $ Pandoc.runIO p
  either (crashWith . show) pure result
