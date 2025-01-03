module Blog.Wiki
  ( buildWiki
  ) where

import qualified Blog.Config as Config
import Blog.Prelude
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Lens as JSON
import qualified Data.Text as T
import qualified Development.Shake as Shake
import Development.Shake.FilePath ((-<.>))
import qualified Development.Shake.FilePath as Shake
import qualified Slick
import qualified Slick.Pandoc as Slick
import qualified Text.Pandoc as Pandoc

buildWiki :: Shake.Action ()
buildWiki =
  void
    $ flip Shake.forP writeWikiTag
    . mergeTags
    . sortOn tagName
    . mconcat
    =<< flip Shake.forP buildWikiPage
    =<< Shake.getDirectoryFiles "." ["site/wiki//*.md"]
 where
  mergeTags :: [Tag] -> [Tag]
  mergeTags =
    \case
      (t1 : t2 : rest)
        | tagName t1 == tagName t2 -> Tag (tagName t1) (tagReference t1 <> tagReference t2) : mergeTags rest
        | otherwise -> t1 : mergeTags (t2 : rest)
      other -> other

data Tag = Tag
  { tagName :: Text
  , tagReference :: [Url]
  }
  deriving stock (Generic)
  deriving anyclass (JSON.ToJSON)

newtype Url = Url Text
  deriving newtype (JSON.ToJSON)

writeWikiTag :: Tag -> Shake.Action ()
writeWikiTag tag = do
  template <- Slick.compileTemplate' "site/template/wiki-tag.html"
  Shake.writeFile' (Config.output </> "wiki" </> "tag" </> T.unpack (tagName tag) -<.> "html")
    . T.unpack
    . Slick.substitute template
    . Config.withMetadataObject "tag"
    $ JSON.toJSON tag

buildWikiPage :: FilePath -> Shake.Action [Tag]
buildWikiPage path = do
  post <-
    Slick.markdownToHTMLWithOpts options Slick.defaultHtml5Options
      . T.pack
      =<< Shake.readFile' path
  let
    url = T.pack . Shake.dropDirectory1 $ path -<.> "html"
  template <- Slick.compileTemplate' "site/template/wiki.html"
  Shake.writeFile' (Config.output </> T.unpack url)
    . T.unpack
    . Slick.substitute template
    . Config.withMetadataObject "wiki"
    $ post
  pure $ post ^.. JSON.key "tags" . JSON._Array . traversed . JSON._String . to (mkTag url)
 where
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

  mkTag :: Text -> Text -> Tag
  mkTag url name =
    Tag
      { tagName = name
      , tagReference = [Url url]
      }
