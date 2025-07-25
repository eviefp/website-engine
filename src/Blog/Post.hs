module Blog.Post
  ( buildPosts
  , Post (..)
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

data Post = Post
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
  }

instance Aeson.FromJSON Post where
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
   where
    toDate :: Aeson.Value -> Parser Date
    toDate (Aeson.String s) =
      case AP.parseOnly (Chronos.parser_Dmy (Just '-')) s of
        Left err -> parseFail $ "failed when parsing date: " <> err
        Right d -> pure d
    toDate invalid = prependFailure "cannot parse date" $ typeMismatch "String" invalid
    go id content publish title author changelog =
      let renderChangelog = not . null $ changelog in Post {..}
  parseJSON invalid = prependFailure "cannot parse post" $ typeMismatch "Object" invalid

instance Aeson.ToJSON Post where
  toJSON Post {..} =
    Aeson.Object
      $ fromList
        [ ("id", Aeson.String id)
        , ("content", Aeson.String content)
        , ("publish", Aeson.String . TL.toStrict . TL.toLazyText . Chronos.builder_Dmy (Just '-') $ publish)
        , ("title", Aeson.String title)
        , ("author", maybe Aeson.Null Aeson.String author)
        , ("changelog", Aeson.Array . Vector.fromList $ Aeson.String <$> changelog)
        , ("renderChangelog", Aeson.Bool . not . null $ changelog)
        ]

parseJSON :: (Aeson.FromJSON a) => Aeson.Value -> Either String a
parseJSON v = case Aeson.fromJSON v of
  Aeson.Success a -> pure a
  Aeson.Error err -> Left err

buildPosts :: Shake.Action [Post]
buildPosts =
  fmap catMaybes
    $ flip Shake.forP buildPostPage
    =<< Shake.getDirectoryFiles "." ["site/post//*.md"]
buildPostPage :: FilePath -> Shake.Action (Maybe Post)
buildPostPage path = do
  post <-
    Slick.markdownToHTMLWithOpts options Slick.defaultHtml5Options
      . T.pack
      =<< Shake.readFile' path
  postData <- either (\err -> fail $ "cannot parse post" <> err) pure $ parseJSON post

  today <- liftIO Chronos.today

  if Chronos.dayToDate today >= publish postData
    then do
      let
        url = "post/" <> T.unpack (id postData) -<.> "html"
      template <- Slick.compileTemplate' "site/template/post.html"
      Shake.writeFile' (Config.output </> url)
        . T.unpack
        . Slick.substitute template
        . Config.withMetadataObject "post"
        . toJSON
        $ postData
      pure $ Just postData
    else pure Nothing
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
