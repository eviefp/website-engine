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
  , RelativePath (..)
  , withMetadataObject
  , addKey
  , takeBaseName
  , options
  , markdownToMetaAndContent
  )
where

import Blog.Item
import Blog.Prelude
import Blog.Settings (Settings (Settings))
import qualified Blog.Settings as Settings
import qualified Blog.Wikilinks as CL

import qualified Chronos
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Functor.Identity (runIdentity)
import qualified Data.Text as T
import qualified Development.Shake as Shake
import qualified Development.Shake.FilePath as Shake
import qualified Slick
import qualified Slick.Pandoc as Slick
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as PW

-- | Will attempt to find the appropriate Rules that match the requested paths.
--
-- You can define rules using @'(%>)'@.
want :: [RelativePath] -> ReaderT Settings Shake.Rules ()
want files = do
  Settings {..} <- ask
  lift . Shake.want $ (output </>) . getRelativePath <$> files

-- | Copy a file from a source-relative path to a destination-relative path.
copyFile :: RelativePath -> RelativePath -> ReaderT Settings Shake.Action ()
copyFile src dest = do
  Settings {..} <- ask
  putInfo $ "[copyFile] " <> getRelativePath src <> " -> " <> getRelativePath dest
  lift
    . Shake.quietly
    $ Shake.copyFileChanged
      (source </> getRelativePath src)
      (output </> getRelativePath dest)

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
  :: String
  -> RelativePath
  -> RelativePath
  -> [(Text, [Item])]
  -> ReaderT Settings Shake.Action ()
generatePage name path templatePath cache = do
  Settings {..} <- ask
  items <- case lookup (T.pack name) cache of
    Nothing -> putError $ "[generatePage] Cannot find item in cache: " <> name
    Just i -> pure i
  case find ((== takeBaseName path) . T.unpack . id) items of
    Nothing ->
      putError
        . join
        $ [ "["
          , name
          , "] Internal error: could not find "
          , name
          , " :"
          , source </> getRelativePath path
          ]
    Just item -> do
      need . fmap (RelativePath . ("tag" </>) . (-<.> "html") . T.unpack) . tags $ item

      putVerbose $ "[generatePage] Generating content for " <> getRelativePath path
      content <- lift . genenerateHtmlWithFixedWikiLinks cache (T.pack name) . documentContent $ item
      putVerbose $ "[generatePage] Generated content:\n" <> show content

      putInfo $ "[generatePage] Generated " <> getRelativePath path
      writeFile templatePath path
        . withMetadataObject name
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
initItemsCache
  :: ReaderT
      Settings
      Shake.Rules
      ((Text, [Shake.FilePattern]) -> ReaderT Settings Shake.Action (Text, [Item]))
initItemsCache = ReaderT $ \Settings {..} -> do
  (fmap . fmap) lift . Shake.newCache $ \(k, path) -> do
    postsPaths <- Shake.getDirectoryFiles source path
    today <- liftIO Chronos.today
    items <-
      Shake.forP
        postsPaths
        ( \postPath ->
            fmap (mkItem today postPath)
              . ( \(meta, blocks) -> do
                    Shake.putVerbose $ "Metadata: \n" <> show meta
                    Shake.putVerbose $ "Content: \n" <> show blocks
                    pure (meta, blocks)
                )
              <=< markdownToMetaAndContent
              . T.pack
              <=< Shake.readFile'
              . (source </>)
              $ postPath
        )
    traverse_ (Shake.putInfo . show) . lefts $ items
    pure . (k,) . rights $ items

-- | Perform mustache substitution and write the new file. Arguments:
--
-- * templatePath: source-relative path for the template
-- * path: destination-relative path for the output file
-- * content: the aeson values to pass as inputs to mustache for substitution
writeFile :: RelativePath -> RelativePath -> Aeson.Value -> ReaderT Settings Shake.Action ()
writeFile templatePath path content = do
  Settings {..} <- ask
  template <- lift . Slick.compileTemplate' $ source </> getRelativePath templatePath
  putInfo
    $ "[writeFile] Writing "
    <> getRelativePath path
    <> " with template "
    <> getRelativePath templatePath
  lift
    . Shake.writeFile' (output </> getRelativePath path)
    . T.unpack
    . Slick.substitute template
    $ content

putVerbose :: String -> ReaderT Settings Shake.Action ()
putVerbose = lift . Shake.putVerbose

putInfo :: String -> ReaderT Settings Shake.Action ()
putInfo = lift . Shake.putInfo

putWarn :: String -> ReaderT Settings Shake.Action ()
putWarn = lift . Shake.putWarn

-- | Logs an error message and also exists the program.
putError :: forall a. String -> ReaderT Settings Shake.Action a
putError err = do
  lift $ Shake.putError err
  crashWith err

-- | Nukes the output directory.
removeOutput :: ReaderT Settings Shake.Action ()
removeOutput = do
  Settings {..} <- ask
  putInfo $ "[removeOutput] Nuking output " <> output
  liftIO . Shake.removeFiles output $ ["//"]

-- | Defines the build rules for an output path.
-- Example:
--
-- @
--     "post/*.html" %> \path -> do
--       ...
--       -- must generate path by the end, otherwise an error will be raised
-- @
(%>)
  :: Shake.FilePattern
  -> (RelativePath -> ReaderT Settings Shake.Action ())
  -> ReaderT Settings Shake.Rules ()
(%>) pat action = do
  settings@Settings {..} <- ask
  lift $ (output </> pat) Shake.%> \path -> do
    Shake.putInfo $ "[%>] Generating " <> path
    flip runReaderT settings
      . action
      . RelativePath
      . Shake.dropDirectory1
      $ path

-- | Declare dependencies between rules through paths.
need :: [RelativePath] -> ReaderT Settings Shake.Action ()
need path = do
  Settings {..} <- ask
  lift . Shake.need . fmap ((output </>) . getRelativePath) $ path

-- | Declare dependencies between rules through paths.
need' :: [Shake.FilePattern] -> ReaderT Settings Shake.Action ()
need' pat = do
  Settings {..} <- ask
  lift do
    Shake.getDirectoryFiles source pat >>= Shake.need . fmap (output </>)

-- | Declare a pseudo-rule using a name rather than a path.
-- This is required because @'(%>)'@ would crash the program if used with a fake name.
-- Can be used to create rules for cleaning or other auxiliary tasks.
(~>) :: String -> ReaderT Settings Shake.Action () -> ReaderT Settings Shake.Rules ()
(~>) name act = do
  settings <- ask
  lift $ name Shake.~> runReaderT act settings

-- | Wrapper for paths. They can be either source-relative or destination-relative.
newtype RelativePath = RelativePath
  { getRelativePath :: FilePath
  }

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
takeBaseName :: RelativePath -> String
takeBaseName = Shake.takeBaseName . getRelativePath

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
  :: (MonadIO m) => (Pandoc.Pandoc -> Pandoc.PandocIO T.Text) -> Pandoc.Meta -> m Aeson.Value
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
  :: [(Text, [Item])] -> Text -> [Pandoc.Block] -> Shake.Action Aeson.Value
genenerateHtmlWithFixedWikiLinks cache def blocks = do
  let
    (result, logs) =
      runIdentity
        . flip State.runStateT []
        . Except.runExceptT
        $ PW.walkM (CL.transform cache def) blocks
  traverse_ CL.printLog logs
  case result of
    Left _ -> crashWith "[Wikilinks] unrecoverable error. Stopping."
    Right fixedBlocks -> do
      let
        writer = Pandoc.writeHtml5String Slick.defaultHtml5Options
        doc = Pandoc.Pandoc mempty fixedBlocks
      outText <- unPandocM $ writer doc
      pure $ Aeson.String outText

unPandocM :: (MonadIO m) => Pandoc.PandocIO a -> m a
unPandocM p = do
  result <- liftIO $ Pandoc.runIO p
  either (crashWith . show) pure result
