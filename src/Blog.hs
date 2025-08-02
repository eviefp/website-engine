module Blog
  ( runNew
  ) where

import qualified Blog.Engine as Engine
import Blog.Prelude
import Blog.Settings (Settings)
import qualified Blog.Settings as Settings

import Control.Monad.Reader (runReaderT)
import qualified Development.Shake as Shake

runNew :: IO ()
runNew = do
  settings <- Settings.parse
  let
    shakeOpts = mkShakeOpts settings
   in
    Shake.shakeArgs shakeOpts do
      runReaderT Engine.run settings
 where
  mkShakeOpts :: Settings -> Shake.ShakeOptions
  mkShakeOpts opts =
    Shake.shakeOptions
      { Shake.shakeLint = Just Shake.LintBasic
      , Shake.shakeTimings = False
      , Shake.shakeLintInside = [Settings.source opts]
      , Shake.shakeColor = True
      , Shake.shakeVerbosity = Shake.Verbose
      , Shake.shakeProgress = Shake.progressSimple
      }
