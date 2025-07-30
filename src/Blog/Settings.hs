module Blog.Settings
  ( Settings (..)
  , parse
  )
where

import Blog.Prelude
import Blog.Settings.Module (Module)

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector as Vector

data Settings = Settings
  { source :: FilePath
  , output :: FilePath
  , modules :: [Module]
  }
  deriving stock (Generic)

instance Aeson.FromJSON Settings where
  parseJSON (Aeson.Object p) =
    go
      <$> p
      .: "source"
      <*> p
      .: "output"
      <*> p
      .: "modules"
   where
    go :: FilePath -> FilePath -> [Module] -> Settings
    go = Settings
  parseJSON invalid = Aeson.prependFailure "cannot parse post" $ Aeson.typeMismatch "Object" invalid

instance Aeson.ToJSON Settings where
  toJSON Settings {..} =
    Aeson.Object
      $ Aeson.fromList
        [ ("source", Aeson.String . T.pack $ source)
        , ("output", Aeson.String . T.pack $ output)
        , ("modules", Aeson.Array . Vector.fromList . fmap Aeson.toJSON $ modules)
        ]

parse :: IO Settings
parse = BL.readFile "website-engine.json" >>= Aeson.throwDecode
