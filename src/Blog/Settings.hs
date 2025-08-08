module Blog.Settings
  ( Settings (..)
  )
where

import Blog.Prelude

import qualified Development.Shake as Shake
import qualified Path as P

data Settings = Settings
  { source :: P.Path P.Rel P.Dir
  , output :: P.Path P.Rel P.Dir
  , verbosity :: Shake.Verbosity
  }
  deriving stock (Generic)
