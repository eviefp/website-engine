module Blog.Engine
  ( want
  , copyFile
  , generatePage
  , initItemsCache
  , writeFile
  , putVerbose
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
  )
where

import Blog.Item
import Blog.Prelude
import Blog.Settings (Settings (Settings))
import qualified Blog.Settings as Settings
import qualified Blog.Wikilinks as CL

import qualified Chronos
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as T
import qualified Development.Shake as Shake
import qualified Development.Shake.FilePath as Shake
import qualified Slick
import qualified Slick.Pandoc as Slick
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as PW

want :: [RelativePath] -> ReaderT Settings Shake.Rules ()
want files = do
  Settings {..} <- ask
  lift . Shake.want $ (output </>) . getRelativePath <$> files

copyFile :: RelativePath -> RelativePath -> ReaderT Settings Shake.Action ()
copyFile src dest = do
  Settings {..} <- ask
  lift
    $ Shake.copyFileChanged
      (source </> getRelativePath src)
      (output </> getRelativePath dest)

generatePage
  :: String
  -> RelativePath
  -> RelativePath
  -> [(Text, [Item])]
  -> ReaderT Settings Shake.Action ()
generatePage name path templatePath cache = do
  Settings {..} <- ask
  items <- case lookup (T.pack name) cache of
    Nothing -> crashWith $ "could not find requested items in cache: " <> name
    Just i -> pure i
  case find ((== takeBaseName path) . T.unpack . id) items of
    Nothing ->
      crashWith
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
      content <- lift . genenerateHtmlWithFixedWikiLinks cache (T.pack name) . documentContent $ item
      writeFile templatePath path
        . withMetadataObject name
        . addKey "content" content
        . Aeson.toJSON
        . metadata
        $ item

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
              . markdownToMetaAndContent
              . T.pack
              <=< Shake.readFile'
              . (source </>)
              $ postPath
        )
    traverse_ (Shake.putVerbose . show) . lefts $ items
    pure . (k,) . rights $ items

writeFile :: RelativePath -> RelativePath -> Aeson.Value -> ReaderT Settings Shake.Action ()
writeFile templatePath path content = do
  Settings {..} <- ask
  template <- lift . Slick.compileTemplate' $ source </> getRelativePath templatePath
  lift
    . Shake.writeFile' (output </> getRelativePath path)
    . T.unpack
    . Slick.substitute template
    $ content

putVerbose :: String -> ReaderT Settings Shake.Action ()
putVerbose = lift . Shake.putVerbose

removeOutput :: ReaderT Settings Shake.Action ()
removeOutput = do
  Settings {..} <- ask
  liftIO . Shake.removeFiles output $ ["//"]

(%>)
  :: Shake.FilePattern
  -> (RelativePath -> ReaderT Settings Shake.Action ())
  -> ReaderT Settings Shake.Rules ()
(%>) pat action = do
  settings@Settings {..} <- ask
  lift $ (output </> pat) Shake.%> \path ->
    flip runReaderT settings
      . action
      . RelativePath
      . Shake.dropDirectory1
      $ path

need :: [RelativePath] -> ReaderT Settings Shake.Action ()
need path = do
  Settings {..} <- ask
  lift . Shake.need . fmap ((output </>) . getRelativePath) $ path

need' :: [Shake.FilePattern] -> ReaderT Settings Shake.Action ()
need' pat = do
  Settings {..} <- ask
  lift do
    Shake.getDirectoryFiles source pat >>= Shake.need . fmap (output </>)

(~>) :: String -> ReaderT Settings Shake.Action () -> ReaderT Settings Shake.Rules ()
(~>) name act = do
  settings <- ask
  lift $ name Shake.~> runReaderT act settings

newtype RelativePath = RelativePath
  { getRelativePath :: FilePath
  }

withMetadataObject :: String -> Aeson.Value -> Aeson.Value
withMetadataObject name json = Aeson.Object $ KeyMap.singleton (Key.fromString name) json

addKey :: String -> Aeson.Value -> Aeson.Value -> Aeson.Value
addKey k v object =
  case object of
    Aeson.Object o -> Aeson.Object . KeyMap.insert (Key.fromString k) v $ o
    other -> crashWith $ "addMetadataObject: json is not an object, found: " <> show other

takeBaseName :: RelativePath -> String
takeBaseName = Shake.takeBaseName . getRelativePath

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

markdownToMetaAndContent :: Text -> Shake.Action (Aeson.Value, [Pandoc.Block])
markdownToMetaAndContent content = do
  let
    writer = Pandoc.writeHtml5String Slick.defaultHtml5Options
  (Pandoc.Pandoc meta' blocks) <- unPandocM $ Pandoc.readMarkdown options content
  meta <- flattenMeta writer meta'
  pure (meta, blocks)

flattenMeta :: (Pandoc.Pandoc -> Pandoc.PandocIO T.Text) -> Pandoc.Meta -> Shake.Action Aeson.Value
flattenMeta writer (Pandoc.Meta meta) = Aeson.toJSON <$> traverse go meta
 where
  go :: Pandoc.MetaValue -> Shake.Action Aeson.Value
  go (Pandoc.MetaMap m) = Aeson.toJSON <$> traverse go m
  go (Pandoc.MetaList m) = Aeson.toJSONList <$> traverse go m
  go (Pandoc.MetaBool m) = pure $ Aeson.toJSON m
  go (Pandoc.MetaString m) = pure $ Aeson.toJSON m
  go (Pandoc.MetaInlines m) = Aeson.toJSON <$> (unPandocM . writer . Pandoc.Pandoc mempty . (: []) . Pandoc.Plain $ m)
  go (Pandoc.MetaBlocks m) = Aeson.toJSON <$> (unPandocM . writer . Pandoc.Pandoc mempty $ m)

genenerateHtmlWithFixedWikiLinks
  :: [(Text, [Item])] -> Text -> [Pandoc.Block] -> Shake.Action Aeson.Value
genenerateHtmlWithFixedWikiLinks cache def blocks = do
  fixedBlocks <- PW.walkM (CL.transform cache def) blocks
  let
    writer = Pandoc.writeHtml5String Slick.defaultHtml5Options
    doc = Pandoc.Pandoc mempty fixedBlocks
  outText <- unPandocM $ writer doc
  pure $ Aeson.String outText

unPandocM :: Pandoc.PandocIO a -> Shake.Action a
unPandocM p = do
  result <- liftIO $ Pandoc.runIO p
  either (crashWith . show) pure result
