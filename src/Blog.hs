module Blog
  ( run
  ) where

import qualified Blog.Engine as Engine
import Blog.Prelude
import Blog.Settings (Settings)
import qualified Blog.Settings as Settings

import Control.Monad.Reader (runReaderT)
import qualified Development.Shake as Shake

run :: IO ()
run = do
  Shake.shakeArgs shakeOpts
    $ runReaderT Engine.run settings
 where
  settings :: Settings
  settings = Settings.Settings {source = "site", output = "docs"}

  shakeOpts :: Shake.ShakeOptions
  shakeOpts =
    Shake.shakeOptions
      { Shake.shakeLint = Just Shake.LintBasic
      , Shake.shakeTimings = False
      , Shake.shakeLintInside = [Settings.source settings]
      , Shake.shakeColor = True
      , Shake.shakeVerbosity = Shake.Verbose
      , Shake.shakeProgress = Shake.progressSimple
      }
