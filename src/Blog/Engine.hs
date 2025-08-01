module Blog.Engine
  ( run
  )
where

import qualified Blog.Config as Config
import Blog.Prelude
import Blog.Settings (Settings)
import qualified Blog.Settings as Settings

import qualified Chronos
import Control.Lens ((^?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Lens (values)
import qualified Data.Attoparsec.Text as AP
import Data.Either (rights)
import Data.Foldable (forM_)
import qualified Data.Text as T
import Development.Shake ((~>))
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
  :: Settings -> Shake.Rules ([Shake.FilePattern] -> Shake.Action [(Item, Aeson.Value)])
initItemsCache Settings.Settings {..} = do
  Shake.newCache $ \path -> do
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

generatePage
  :: Settings
  -> String
  -> RelativePath
  -> FilePath
  -> [(Item, Aeson.Value)]
  -> Shake.Action ()
generatePage Settings.Settings {..} name path templatePath pages = do
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
      template <- Slick.compileTemplate' templatePath
      writeFile path
        . T.unpack
        . Slick.substitute template
        . Config.withMetadataObject name
        . Aeson.toJSON
        . snd
        $ pageData
 where
  takeBaseName :: RelativePath -> String
  takeBaseName = Shake.takeBaseName . getRelativePath

  need :: [RelativePath] -> Shake.Action ()
  need = Shake.need . fmap ((output </>) . getRelativePath)

  writeFile :: RelativePath -> String -> Shake.Action ()
  writeFile p = Shake.writeFile' (output </> getRelativePath p)

run :: Settings -> Shake.Rules ()
run settings@Settings.Settings {..} = do
  itemsCache <- initItemsCache settings

  want [RelativePath "index.html"]

  "clean" ~> removeOutput

  -- index
  "index.html" %> \path -> do
    need' ["css//*", "images//*"]

    posts <- itemsCache ["post//*.md"]
    Shake.putVerbose $ "found " <> show (length posts) <> " post(s)"

    pages <- itemsCache ["page//*.md"]
    Shake.putVerbose $ "found " <> show (length posts) <> " page(s)"

    wikis <- itemsCache ["wiki//*.md"]
    Shake.putVerbose $ "found " <> show (length posts) <> " wiki page(s)"

    need . fmap (\post -> RelativePath $ "post/" </> (T.unpack . id . fst $ post) -<.> "html") $ posts
    need . fmap (\post -> RelativePath $ "page/" </> (T.unpack . id . fst $ post) -<.> "html") $ pages
    need . fmap (\post -> RelativePath $ "wiki/" </> (T.unpack . id . fst $ post) -<.> "html") $ wikis

    let
      sortedPosts = sortOn (Down . publish . fst) posts
    template <- Slick.compileTemplate' (source </> "template/index.html")
    writeFile path
      . T.unpack
      . Slick.substitute template
      . Config.withMetadataObject "posts"
      . Aeson.toJSON
      . fmap snd
      $ sortedPosts

  -- static content
  "css//*" %> \path ->
    copyFile path path

  "images//*" %> \path ->
    copyFile path path

  -- posts
  "post//*.html" %> \path -> do
    need' ["post/content//*"]
    itemsCache ["post//*.md"] >>= generatePage settings "post" path "site/template/post.html"
  "post/content//*" %> \path ->
    copyFile path path

  -- pages
  "page//*.html" %> \path -> do
    need' ["page/content//*"]
    itemsCache ["page//*.md"] >>= generatePage settings "page" path "site/template/page.html"
  "page/content//*" %> \path ->
    copyFile path path

  -- wiki
  "wiki//*.html" %> \path -> do
    need' ["wiki/content//*"]
    itemsCache ["wiki//*.md"] >>= generatePage settings "wiki" path "site/template/wiki.html"
  "wiki/content//*" %> \path ->
    copyFile path path

  -- tags
  "tag/*.html" %> \path -> do
    allPosts <- itemsCache ["post//*.md"]
    allPages <- itemsCache ["page//*.md"]
    allWikis <- itemsCache ["wiki//*.md"]
    let
      tagName = T.pack . takeBaseName $ path
    let
      posts = filter ((tagName `elem`) . tags . fst) allPosts
      pages = filter ((tagName `elem`) . tags . fst) allPages
      wikis = filter ((tagName `elem`) . tags . fst) allWikis

    template <- Slick.compileTemplate' "site/template/tag.html"
    writeFile path
      . T.unpack
      . Slick.substitute template
      . Config.addKey "posts" (Aeson.toJSON $ fmap snd posts)
      . Config.addKey "pages" (Aeson.toJSON $ fmap snd pages)
      . Config.addKey "wikis" (Aeson.toJSON $ fmap snd wikis)
      . Config.withMetadataObject "tagName"
      . Aeson.toJSON
      $ tagName
 where
  want :: [RelativePath] -> Shake.Rules ()
  want files = Shake.want $ (output </>) . getRelativePath <$> files

  need :: [RelativePath] -> Shake.Action ()
  need = Shake.need . fmap ((output </>) . getRelativePath)

  need' :: [Shake.FilePattern] -> Shake.Action ()
  need' pat = Shake.getDirectoryFiles source pat >>= Shake.need . fmap (output </>)

  copyFile :: RelativePath -> RelativePath -> Shake.Action ()
  copyFile src dest = Shake.copyFileChanged (source </> getRelativePath src) (output </> getRelativePath dest)

  writeFile :: RelativePath -> String -> Shake.Action ()
  writeFile path = Shake.writeFile' (output </> getRelativePath path)

  removeOutput :: Shake.Action ()
  removeOutput = liftIO . Shake.removeFiles output $ ["//"]

  (%>) :: Shake.FilePattern -> (RelativePath -> Shake.Action ()) -> Shake.Rules ()
  (%>) pat act = (output </> pat) Shake.%> \path -> act (RelativePath $ Shake.dropDirectory1 path)

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
