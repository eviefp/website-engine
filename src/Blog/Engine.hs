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
import qualified Data.Attoparsec.Text as AP
import Data.Data (Typeable)
import qualified Data.Text as T
import qualified Development.Shake as Shake
import Development.Shake.Classes (Binary, Hashable, NFData)
import Development.Shake.FilePath ((-<.>))
import qualified Development.Shake.FilePath as Shake
import qualified Slick
import qualified Slick.Pandoc as Slick
import qualified Text.Pandoc as Pandoc

data Item = Item
  { id :: Text
  , title :: Text
  , publish :: Date
  }
  deriving stock (Show)

newtype CurrentDate = CurrentDate ()
  deriving newtype (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance Shake.RuleResult CurrentDate = Chronos.Day

mkItem :: Chronos.Day -> Aeson.Value -> Maybe (Item, Aeson.Value)
mkItem day v = do
  id <- v ^? key "id" . _String
  title <- v ^? key "title" . _String
  pub <- v ^? key "publish" . _String
  publish <- case AP.parseOnly (Chronos.parser_Dmy (Just '-')) pub of
    Left _ -> Nothing
    Right d -> pure d
  if Chronos.dayToDate day < publish
    then Nothing
    else
      pure (Item {..}, v)

run :: Settings -> Shake.Rules ()
run Settings.Settings {..} = do
  want ["index.html"]

  getToday <- Shake.newCache $ \() ->
    liftIO Chronos.today

  postsCache <- Shake.newCache $ \path -> do
    postsPaths <- Shake.getDirectoryFiles source path
    today <- getToday ()
    catMaybes
      <$> Shake.forP
        postsPaths
        ( Shake.readFile'
            . (source </>)
            >=> fmap (mkItem today)
            . Slick.markdownToHTMLWithOpts options Slick.defaultHtml5Options
            . T.pack
        )

  -- index
  "index.html" %> \_ -> do
    need' ["css//*", "images//*"]

    posts <- postsCache ["post//*.md"]
    Shake.putVerbose $ "found " <> show (length posts) <> " post(s)"
    Shake.need . fmap (\post -> "docs/post/" </> (T.unpack . id . fst $ post) -<.> "html") $ posts

    let
      template = source </> "template/index.html"
      sortedPosts = sortOn (Down . publish . fst) posts
    indexT <- Slick.compileTemplate' template
    Shake.writeFile' (output </> "index.html")
      . T.unpack
      . Slick.substitute indexT
      . Config.withMetadataObject "posts"
      . Aeson.toJSON
      . fmap snd
      $ sortedPosts

  -- static content
  "css//*" %> \path -> do
    copyFile (Shake.dropDirectory1 path) (Shake.dropDirectory1 path)
  "images//*" %> \path -> do
    copyFile (Shake.dropDirectory1 path) (Shake.dropDirectory1 path)
  -- todo: images for posts
  -- posts
  "post//*.html" %> \path -> do
    posts <- postsCache ["post//*.md"]
    case find ((== Shake.takeBaseName path) . T.unpack . id . fst) posts of
      Nothing -> fail $ "[post] Internal error: could not find post " <> path
      Just postData -> do
        template <- Slick.compileTemplate' "site/template/post.html"
        Shake.writeFile' path
          . T.unpack
          . Slick.substitute template
          . Config.withMetadataObject "post"
          . Aeson.toJSON
          . snd
          $ postData
 where
  want :: [FilePath] -> Shake.Rules ()
  want files = Shake.want $ (output </>) <$> files

  need' :: [Shake.FilePattern] -> Shake.Action ()
  need' pat = Shake.getDirectoryFiles source pat >>= Shake.need . fmap (output </>)

  copyFile :: FilePath -> FilePath -> Shake.Action ()
  copyFile src dest = Shake.copyFileChanged (source </> src) (output </> dest)

  (%>) :: Shake.FilePattern -> (FilePath -> Shake.Action ()) -> Shake.Rules ()
  (%>) pat act = (output </> pat) Shake.%> act

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
