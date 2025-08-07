module Blog.Types
  ( Rules
  , Action
  , ItemKind (..)
  , Log (..)
  , printLog
  ) where

import Blog.Prelude
import Blog.Settings (Settings)

import Control.Exception.Extra (Partial)
import Data.String (IsString)
import qualified Development.Shake as Shake
import Development.Shake.Classes (Hashable)
import qualified Development.Shake.Plus as SP

-- | Define rules for building.
-- See @'(%>)'@, @'(~>)'@, and 'initItemsCache'.
type Rules a = SP.ShakePlus Settings a

-- | Define how to build files/paths/etc.
-- See 'copyFile', 'generatePage', 'need', etc.
type Action a = SP.RAction Settings a

newtype ItemKind = ItemKind
  { getItemKind :: Text
  }
  deriving newtype (Show, Eq, Hashable, IsString)

-- A few thoughts about this type
--   - no show instance
--   - fixed to strings
--   - not sure this should be in this module
-- However, I'm not sure how this wants to evolve yet.
-- We'll know more when we need it again.
data Log
  = Verbose String
  | Warning String
  | Error String

printLog :: (Partial) => Log -> Action ()
printLog =
  \case
    Verbose str -> SP.liftAction $ Shake.putVerbose str
    Warning str -> SP.liftAction $ Shake.putWarn str
    Error str -> SP.liftAction $ Shake.putError str
