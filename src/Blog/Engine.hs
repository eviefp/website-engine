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
import qualified Data.Text as T
import qualified Development.Shake as Shake
import qualified Slick
import qualified Slick.Pandoc as Slick
import qualified Text.Pandoc as Pandoc

data Item = Item
  { id :: Text
  , title :: Text
  , publish :: Date
  }
  deriving stock (Show)

mkItem :: Aeson.Value -> Maybe (Item, Aeson.Value)
mkItem v = do
  id <- v ^? key "id" . _String
  title <- v ^? key "title" . _String
  pub <- v ^? key "publish" . _String
  publish <- case AP.parseOnly (Chronos.parser_Dmy (Just '-')) pub of
    Left _ -> Nothing
    Right d -> pure d
  pure (Item {..}, v)

run :: Settings -> Shake.Rules ()
run Settings.Settings {..} = do
  want ["index.html"]

  postsCache <- Shake.newCache $ \path -> do
    postsPaths <- Shake.getDirectoryFiles source path
    catMaybes
      <$> Shake.forP
        postsPaths
        ( Shake.readFile'
            . (source </>)
            >=> fmap mkItem
            . Slick.markdownToHTMLWithOpts options Slick.defaultHtml5Options
            . T.pack
        )

  "index.html" %> \_ -> do
    posts <- postsCache ["post//*.md"]
    Shake.putVerbose $ "found posts: " <> show posts
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
    pure ()
 where
  want :: [FilePath] -> Shake.Rules ()
  want files = Shake.want $ (output </>) <$> files

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
