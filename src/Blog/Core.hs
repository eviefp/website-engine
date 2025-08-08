module Blog.Core
  ( want
  , (%>)
  , need
  , need'
  , needItems
  , (~>)
  , putInfo
  , putVerbose
  , putWarn
  , putError
  , quietly
  )
where

import Blog.Item
import Blog.Path.Rel as Rel
import Blog.Prelude
import qualified Blog.Settings as Settings
import Blog.Types

import Control.Exception.Extra (Partial)
import qualified Data.Text as T
import qualified Development.Shake as Shake
import qualified Development.Shake.Plus as SP
import qualified System.FilePath as U

-- | Will attempt to find the appropriate Rules that match the requested paths.
--
-- You can define rules using @'(%>)'@.
--
-- 't' can be list, Maybe, etc.
want :: (Partial, Traversable t) => t (Path Rel.Output File) -> Rules ()
want files = traverse outputToRel files >>= SP.wantP

-- | Defines the build rules for an output path.
-- Example:
--
-- @
--     "post/*.html" %> \path -> do
--       ...
--       -- must generate path by the end, otherwise an error will be raised
-- @
(%>) :: Shake.FilePattern -> (Path Rel.Output File -> Action ()) -> Rules ()
(%>) pat action = do
  output <- asks Settings.output
  (toFilePath output U.</> pat) SP.%> \path -> do
    putInfo $ "[%>] Generating " <> show path
    p <- stripProperPrefix output path
    action (asOutput p)

-- | Declare dependencies between rules through paths.
need :: (Traversable t) => t (Path Rel.Output File) -> Action ()
need path = traverse outputToRel path >>= SP.needP

-- | Declare dependencies between rules through paths.
need' :: [Shake.FilePattern] -> Action ()
need' pat = do
  source <- asks Settings.source
  SP.getDirectoryFiles source pat >>= need . fmap asOutput

-- | Needs all items in a cache.
-- Assumes that the output path is @<ItemKind>/<itemId.html>@
needItems :: (ItemKind, [Item]) -> Action ()
needItems (itemKind, items) = do
  itemDir <- fmap asOutput . parseRelDir . T.unpack . getItemKind $ itemKind
  need
    . fmap (itemDir </>)
    <=< traverse (addExtension ".html")
    <=< traverse parseRelFile
    . fmap (T.unpack . getItemId . id)
    $ items

-- | Declare a pseudo-rule using a name rather than a path.
-- This is required because @'(%>)'@ would crash the program if used with a fake name.
-- Can be used to create rules for cleaning or other auxiliary tasks.
(~>) :: String -> Action () -> Rules ()
(~>) = SP.phony

putVerbose :: String -> Action ()
putVerbose = SP.liftAction . Shake.putVerbose

putInfo :: String -> Action ()
putInfo = SP.liftAction . Shake.putInfo

putWarn :: String -> Action ()
putWarn = SP.liftAction . Shake.putWarn

-- | Logs an error message and also exits the program.
putError :: (Partial) => String -> Action a
putError err = do
  SP.liftAction $ Shake.putError err
  crashWith err

quietly :: Action a -> Action a
quietly act = SP.withRunInAction (\run -> run act)
