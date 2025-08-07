module Blog.Path
  ( OutputRel
  , SourceRel
  , asOutputRel
  , asSourceRel
  , outputToRel
  , sourceToRel
  ) where

import Blog.Prelude
import Blog.Settings (Settings (..))

import Control.Monad.Reader.Class
import qualified Path.Internal.Posix as PI
import qualified Path.Posix as P

-- | Output-relative path.
data OutputRel

-- | Source-relative path.
data SourceRel

-- | This path should be assumed to be relative to the output.
-- Its internal representation/path does *NOT* include the output directory,
-- but will signal to 'Blog.Engine' functions to prepend it.
asOutputRel :: P.Path b t -> P.Path OutputRel t
asOutputRel (PI.Path p) = PI.Path p

-- | This path should be assumed to be relative to the source.
-- Its internal representation/path does *NOT* include the source directory,
-- but will signal to 'Blog.Engine' functions to prepend it.
asSourceRel :: P.Path b t -> P.Path SourceRel t
asSourceRel (PI.Path p) = PI.Path p

-- | Prepend the output path.
-- 'm' can be 'Action' or 'Rules'.
outputToRel :: (MonadReader Settings m) => P.Path OutputRel t -> m (P.Path P.Rel t)
outputToRel (PI.Path p) = (P.</> PI.Path p) <$> asks output

-- | Prepend the source path.
-- 'm' can be 'Action' or 'Rules'.
sourceToRel :: (MonadReader Settings m) => P.Path SourceRel t -> m (P.Path P.Rel t)
sourceToRel (PI.Path p) = (P.</> PI.Path p) <$> asks source
