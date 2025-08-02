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
  , (<!>)
  , (~>)
  , MetadataError (..)
  , Item (..)
  , RelativePath (..)
  , withMetadataObject
  , addKey
  , takeBaseName
  , options
  )
where

import Blog.Prelude
import Blog.Settings (Settings (Settings))
import qualified Blog.Settings as Settings

import qualified Chronos
import Control.Lens ((^?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Attoparsec.Text as AP
import qualified Data.Text as T
import qualified Development.Shake as Shake
import qualified Development.Shake.FilePath as Shake
import qualified Slick
import qualified Slick.Pandoc as Slick
import qualified Text.Pandoc as Pandoc

want :: [RelativePath] -> ReaderT Settings Shake.Rules ()
want files = do
  Settings {..} <- ask
  lift . Shake.want $ (output </>) . getRelativePath <$> files

copyFile :: RelativePath -> RelativePath -> ReaderT Settings Shake.Action ()
copyFile src dest = do
  Settings {..} <- ask
  lift $ Shake.copyFileChanged (source </> getRelativePath src) (output </> getRelativePath dest)

generatePage
  :: String
  -> RelativePath
  -> RelativePath
  -> [(Item, Aeson.Value)]
  -> ReaderT Settings Shake.Action ()
generatePage name path templatePath pages = do
  Settings {..} <- ask
  case find ((== takeBaseName path) . T.unpack . id . fst) pages of
    Nothing ->
      fail
        $ "["
        <> name
        <> "] Internal error: could not find "
        <> name
        <> " :"
        <> (source </> getRelativePath path)
    Just pageData -> do
      need . fmap (RelativePath . (\t -> "tag/" </> t -<.> "html") . T.unpack) . tags . fst $ pageData
      writeFile templatePath path
        . withMetadataObject name
        . Aeson.toJSON
        . snd
        $ pageData

initItemsCache
  :: ReaderT
      Settings
      Shake.Rules
      ([Shake.FilePattern] -> ReaderT Settings Shake.Action [(Item, Aeson.Value)])
initItemsCache = ReaderT $ \Settings {..} -> do
  fn <- Shake.newCache $ \path -> do
    postsPaths <- Shake.getDirectoryFiles source path
    today <- liftIO Chronos.today

    items <-
      Shake.forP
        postsPaths
        ( \pp ->
            Shake.readFile'
              . (source </>)
              >=> fmap (mkItem today pp)
              . Slick.markdownToHTMLWithOpts options Slick.defaultHtml5Options
              . T.pack
              $ pp
        )
    forM_ items \case
      Left err -> Shake.putVerbose $ show err
      Right _ -> pure ()
    pure . rights $ items
  pure $ \fp -> lift $ fn fp

writeFile :: RelativePath -> RelativePath -> Aeson.Value -> ReaderT Settings Shake.Action ()
writeFile templatePath path content = do
  Settings {..} <- ask
  template <- lift $ Slick.compileTemplate' (source </> getRelativePath templatePath)
  lift
    $ Shake.writeFile' (output </> getRelativePath path)
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
(%>) pat act = do
  settings@Settings {..} <- ask
  lift $ (output </> pat) Shake.%> \path ->
    flip runReaderT settings $ act (RelativePath $ Shake.dropDirectory1 path)

need :: [RelativePath] -> ReaderT Settings Shake.Action ()
need path = do
  Settings {..} <- ask
  lift . Shake.need . fmap ((output </>) . getRelativePath) $ path

need' :: [Shake.FilePattern] -> ReaderT Settings Shake.Action ()
need' pat = do
  Settings {..} <- ask
  lift $ Shake.getDirectoryFiles source pat >>= Shake.need . fmap (output </>)

(<!>) :: Maybe b -> a -> Either a b
(<!>) mb err = maybe (Left err) Right mb
infixr 7 <!>

(~>) :: String -> ReaderT Settings Shake.Action () -> ReaderT Settings Shake.Rules ()
(~>) name act = do
  settings <- ask
  lift $ name Shake.~> runReaderT act settings

newtype RelativePath = RelativePath
  { getRelativePath :: FilePath
  }

data Item = Item
  { id :: Text
  , title :: Text
  , publish :: Date
  , tags :: [Text]
  }

data MetadataError
  = MissingKey FilePath String
  | MalformedPublishDate FilePath String
  | PostNotPublishedYet FilePath Text
  deriving stock (Show)

withMetadataObject :: String -> Aeson.Value -> Aeson.Value
withMetadataObject name json = Aeson.Object . KeyMap.insert (Key.fromString name) json $ KeyMap.empty

addKey :: String -> Aeson.Value -> Aeson.Value -> Aeson.Value
addKey k v object =
  case object of
    Aeson.Object o -> Aeson.Object . KeyMap.insert (Key.fromString k) v $ o
    _ -> crashWith "addMetadataObject: json is not object"

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

-- perhaps use something other than `Maybe` and upstream the error/reason?
-- I'd like to see "skipped because future", "could not parse date" as an error etc
mkItem :: Chronos.Day -> FilePath -> Aeson.Value -> Either MetadataError (Item, Aeson.Value)
mkItem day p v = do
  id <- v ^? key "id" . _String <!> MissingKey p "id"
  title <- v ^? key "title" . _String <!> MissingKey p "title"
  pub <- v ^? key "publish" . _String <!> MissingKey p "publish"
  publish <- case AP.parseOnly (Chronos.parser_Dmy (Just '-')) pub of
    Left err -> Left $ MalformedPublishDate p err
    Right d -> pure d
  let
    tags = v ^.. key "tags" . values . _String
  if Chronos.dayToDate day < publish
    then Left $ PostNotPublishedYet p id
    else
      let
        changelog = v ^? key "changelog" . _Array
        -- if `renderChangelog` is present, return that value
        -- if not, if `changelog` is not present, return False
        -- else, return whether the changelog is not empty
        renderChangelog = maybe (maybe False (not . null) changelog) identity $ v ^? key "renderChangelog" . _Bool
        renderChangelog' = Aeson.Bool . maybe renderChangelog ((&& renderChangelog) . not . null) $ changelog
        value = case v of
          Aeson.Object kv -> Aeson.Object $ KeyMap.insert "renderChangelog" renderChangelog' kv
          _ -> v
      in
        pure (Item {..}, value)
