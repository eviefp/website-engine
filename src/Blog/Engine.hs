module Blog.Engine
  ( (%>)
  , (<!>)
  , (~>)
  , Item (..)
  , MetadataError (..)
  , RelativePath (..)
  , copyFile
  , generatePage
  , initItemsCache
  , need
  , need'
  , options
  , putVerbose
  , removeOutput
  , run
  , takeBaseName
  , want
  , writeFile
  )
where

import Blog.Prelude
import Blog.Settings (Settings)
import qualified Blog.Settings as Settings

import qualified Chronos
import Control.Lens ((^?))
import Control.Monad.Reader (ReaderT (ReaderT), ask, runReaderT)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Lens (values)
import qualified Data.Attoparsec.Text as AP
import Data.Either (rights)
import Data.Foldable (forM_)
import qualified Data.Text as T
import qualified Development.Shake as Shake
import Development.Shake.FilePath ((-<.>))
import qualified Development.Shake.FilePath as Shake
import qualified Slick
import qualified Slick.Pandoc as Slick
import qualified Text.Pandoc as Pandoc

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

(<!>) :: Maybe b -> a -> Either a b
(<!>) mb err = maybe (Left err) Right mb
infixr 7 <!>

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
        renderChangelog = maybe False identity $ v ^? key "renderChangelog" . _Bool
        renderChangelog' = Aeson.Bool . maybe renderChangelog ((&& renderChangelog) . not . null) $ changelog
        value = case v of
          Aeson.Object kv -> Aeson.Object $ KeyMap.insert "renderChangelog" renderChangelog' kv
          _ -> v
      in
        pure (Item {..}, value)

initItemsCache
  :: ReaderT
      Settings
      Shake.Rules
      ([Shake.FilePattern] -> ReaderT Settings Shake.Action [(Item, Aeson.Value)])
initItemsCache = ReaderT $ \Settings.Settings {..} -> do
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

generatePage
  :: String
  -> RelativePath
  -> RelativePath
  -> [(Item, Aeson.Value)]
  -> ReaderT Settings Shake.Action ()
generatePage name path templatePath pages = do
  Settings.Settings {..} <- ask
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

removeOutput :: ReaderT Settings Shake.Action ()
removeOutput = do
  Settings.Settings {..} <- ask
  liftIO . Shake.removeFiles output $ ["//"]

want :: [RelativePath] -> ReaderT Settings Shake.Rules ()
want files = do
  Settings.Settings {..} <- ask
  lift . Shake.want $ (output </>) . getRelativePath <$> files

(~>) :: String -> ReaderT Settings Shake.Action () -> ReaderT Settings Shake.Rules ()
(~>) name act = do
  settings <- ask
  lift $ name Shake.~> runReaderT act settings

(%>)
  :: Shake.FilePattern
  -> (RelativePath -> ReaderT Settings Shake.Action ())
  -> ReaderT Settings Shake.Rules ()
(%>) pat act = do
  settings@Settings.Settings {..} <- ask
  lift $ (output </> pat) Shake.%> \path ->
    flip runReaderT settings $ act (RelativePath $ Shake.dropDirectory1 path)

need' :: [Shake.FilePattern] -> ReaderT Settings Shake.Action ()
need' pat = do
  Settings.Settings {..} <- ask
  lift $ Shake.getDirectoryFiles source pat >>= Shake.need . fmap (output </>)

need :: [RelativePath] -> ReaderT Settings Shake.Action ()
need path = do
  Settings.Settings {..} <- ask
  lift . Shake.need . fmap ((output </>) . getRelativePath) $ path

putVerbose :: String -> ReaderT Settings Shake.Action ()
putVerbose = lift . Shake.putVerbose

writeFile :: RelativePath -> RelativePath -> Aeson.Value -> ReaderT Settings Shake.Action ()
writeFile templatePath path content = do
  Settings.Settings {..} <- ask
  template <- lift $ Slick.compileTemplate' (source </> getRelativePath templatePath)
  lift
    $ Shake.writeFile' (output </> getRelativePath path)
    . T.unpack
    . Slick.substitute template
    $ content

copyFile :: RelativePath -> RelativePath -> ReaderT Settings Shake.Action ()
copyFile src dest = do
  Settings.Settings {..} <- ask
  lift $ Shake.copyFileChanged (source </> getRelativePath src) (output </> getRelativePath dest)

takeBaseName :: RelativePath -> String
takeBaseName = Shake.takeBaseName . getRelativePath

withMetadataObject :: String -> Aeson.Value -> Aeson.Value
withMetadataObject name json = Aeson.Object . KeyMap.insert (Key.fromString name) json $ KeyMap.empty

addKey :: String -> Aeson.Value -> Aeson.Value -> Aeson.Value
addKey k v object =
  case object of
    Aeson.Object o -> Aeson.Object . KeyMap.insert (Key.fromString k) v $ o
    _ -> crashWith "addMetadataObject: json is not object"

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
