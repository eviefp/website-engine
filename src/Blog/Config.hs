module Blog.Config
  ( Metadata
  , metadata
  ) where

import Blog.Prelude
import qualified Data.Aeson as JSON

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
