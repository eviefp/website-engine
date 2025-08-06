module Blog.Settings
  ( Settings (..)
  )
where

import Blog.Prelude

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T

-- | Used by 'RelativePath' to describe source and output relative paths.
data Settings = Settings
  { source :: FilePath
  , output :: FilePath
  }
  deriving stock (Generic)

instance Aeson.FromJSON Settings where
  parseJSON (Aeson.Object p) =
    go
      <$> p
      .: "source"
      <*> p
      .: "output"
   where
    go :: FilePath -> FilePath -> Settings
    go = Settings
  parseJSON invalid = Aeson.prependFailure "cannot parse settings" $ Aeson.typeMismatch "Object" invalid

instance Aeson.ToJSON Settings where
  toJSON Settings {..} =
    Aeson.Object
      $ Aeson.fromList
        [ ("source", Aeson.String . T.pack $ source)
        , ("output", Aeson.String . T.pack $ output)
        ]
