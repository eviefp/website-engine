module Blog.Pages
  ( buildPages
  ) where

import qualified Blog.Config as Config
import Blog.Prelude
import qualified Chronos
import Data.Aeson (ToJSON (toJSON), (.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.KeyMap (fromList)
import Data.Aeson.Types (Parser, parseFail, prependFailure, typeMismatch)
import qualified Data.Attoparsec.Text as AP
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Vector as Vector
import qualified Development.Shake as Shake
import Development.Shake.FilePath ((-<.>))
import qualified Slick
import qualified Slick.Pandoc as Slick
import qualified Text.Pandoc as Pandoc

data Page = Page
  { id :: Text
  , content :: Text
  , publish :: Date
  , title :: Text
  , author :: Maybe Text
  , changelog :: [Text]
  , renderChangelog :: Bool
  -- ^ This is a hack because the moustache library doesn't have any way to test
  -- whether an array is not empty, and I couldn't figure out a nicer way to
  -- not print the changelog header.
  , url :: Maybe Text
  }

instance Aeson.FromJSON Page where
  parseJSON (Aeson.Object p) =
    go
      <$> p
      .: "id"
      <*> p
      .: "content"
      <*> (p .: "publish" >>= toDate)
      <*> p
      .: "title"
      <*> p
      .:? "author"
      <*> p
      .:? "changelog"
      .!= []
      <*> p
      .:? "url"
   where
    toDate :: Aeson.Value -> Parser Date
    toDate (Aeson.String s) =
      case AP.parseOnly (Chronos.parser_Dmy (Just '-')) s of
        Left err -> parseFail $ "failed when parsing date: " <> err
        Right d -> pure d
    toDate invalid = prependFailure "cannot parse date" $ typeMismatch "String" invalid
    go id content publish title author changelog url =
      let renderChangelog = not . null $ changelog in Page {..}
  parseJSON invalid = prependFailure "cannot parse post" $ typeMismatch "Object" invalid

instance Aeson.ToJSON Page where
  toJSON Page {..} =
    Aeson.Object
      $ fromList
        [ ("id", Aeson.String id)
        , ("content", Aeson.String content)
        , ("publish", Aeson.String . TL.toStrict . TL.toLazyText . Chronos.builder_Dmy (Just '-') $ publish)
        , ("title", Aeson.String title)
        , ("author", maybe Aeson.Null Aeson.String author)
        , ("changelog", Aeson.Array . Vector.fromList $ Aeson.String <$> changelog)
        , ("renderChangelog", Aeson.Bool . not . null $ changelog)
        , ("url", maybe Aeson.Null Aeson.String url)
        ]

buildPages :: Shake.Action ()
buildPages =
  Shake.getDirectoryFiles "." ["site/page//**.md"]
    >>= void
    . flip Shake.forP buildPage

buildPage :: FilePath -> Shake.Action ()
buildPage path = do
  page <-
    Slick.markdownToHTMLWithOpts options Slick.defaultHtml5Options
      . T.pack
      =<< Shake.readFile' path
  pageData <- either (\err -> fail $ "cannot parse post" <> err) pure $ parseJSON page

  today <- liftIO Chronos.today

  when (Chronos.dayToDate today >= publish pageData) $ do
    let
      outputUrl = maybe ("page/" <> T.unpack (id pageData) -<.> "html") T.unpack (url pageData)
    template <- Slick.compileTemplate' "site/template/page.html"
    Shake.writeFile' (Config.output </> outputUrl)
      . T.unpack
      . Slick.substitute template
      . Config.withMetadataObject "page"
      . toJSON
      $ pageData
    pure ()
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
