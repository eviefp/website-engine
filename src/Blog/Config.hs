module Blog.Config
  ( Metadata
  , metadata
  , withMetadataObject
  , addKey
  , output
  ) where

import Blog.Prelude
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified GHC.Err as Unsafe

output :: FilePath
output = "generated"

newtype Metadata = Metadata
  { title :: Text
  }
  deriving stock (Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

metadata :: Metadata
metadata =
  Metadata
    { title = "Evie"
    }

withMetadataObject :: String -> JSON.Value -> JSON.Value
withMetadataObject name json =
  case JSON.toJSON metadata of
    JSON.Object m -> JSON.Object . KeyMap.insert (Key.fromString name) json $ m
    _ -> Unsafe.error "withMetadata: json is not object"

addKey :: String -> JSON.Value -> JSON.Value -> JSON.Value
addKey key value object =
  case object of
    JSON.Object o -> JSON.Object . KeyMap.insert (Key.fromString key) value $ o
    _ -> Unsafe.error "addMetadataObject: json is not object"

