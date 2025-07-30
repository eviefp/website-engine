module Blog.Settings.Module
  ( Module (..)
  , MustachePattern (..)
  )
where

import Blog.Prelude
import Blog.Settings.Tag (Tag)

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T

newtype MustachePattern = MustachePattern Text
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)
  deriving stock (Generic)

data Module = Module
  { name :: Text
  , files :: FilePath
  , output :: MustachePattern
  , template :: FilePath
  , tags :: Maybe Tag
  }
  deriving stock (Generic)

instance Aeson.FromJSON Module where
  parseJSON (Aeson.Object p) =
    go
      <$> p
      .: "name"
      <*> p
      .: "files"
      <*> p
      .: "output"
      <*> p
      .: "template"
      <*> p
      .:? "tags"
   where
    go :: Text -> FilePath -> MustachePattern -> FilePath -> Maybe Tag -> Module
    go = Module
  parseJSON invalid = Aeson.prependFailure "cannot parse post" $ Aeson.typeMismatch "Object" invalid

instance Aeson.ToJSON Module where
  toJSON Module {..} =
    Aeson.Object
      $ Aeson.fromList
        [ ("name", Aeson.String name)
        , ("files", Aeson.String . T.pack $ files)
        , ("output", Aeson.toJSON output)
        , ("template", Aeson.String . T.pack $ template)
        , ("tags", maybe Aeson.Null Aeson.toJSON tags)
        ]
