module Blog.Settings.Tag
  ( Tag (..)
  )
where

import Blog.Prelude

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T

data Tag = Tag
  { template :: FilePath
  , output :: FilePath
  , field :: Text
  }
  deriving stock (Generic)

instance Aeson.FromJSON Tag where
  parseJSON (Aeson.Object p) =
    go
      <$> p
      .: "template"
      <*> p
      .: "output"
      <*> p
      .: "field"
   where
    go :: FilePath -> FilePath -> Text -> Tag
    go = Tag
  parseJSON invalid = Aeson.prependFailure "cannot parse post" $ Aeson.typeMismatch "Object" invalid

instance Aeson.ToJSON Tag where
  toJSON Tag {..} =
    Aeson.Object
      $ Aeson.fromList
        [ ("template", Aeson.String . T.pack $ template)
        , ("output", Aeson.String . T.pack $ output)
        , ("field", Aeson.String field)
        ]
