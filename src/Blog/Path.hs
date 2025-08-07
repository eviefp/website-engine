module Blog.Path
  ( OutputRel
  , SourceRel
  , asOutputRel
  , asSourceRel
  , outputToRel
  , sourceToRel
  , outputRelFile
  , outputRelDir
  , sourceRelFile
  , sourceRelDir
  , pathToString
  ) where

import Blog.Prelude
import Blog.Settings (Settings (..))

import Control.Monad.Reader.Class
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import qualified Language.Haskell.TH.Syntax as TH
import qualified Path.Internal.Posix as PI
import qualified Path.Posix as P

-- | Output-relative path.
data OutputRel

-- | Source-relative path.
data SourceRel

pathToString :: P.Path b t -> FilePath
pathToString (PI.Path p) = stripPrefix p
 where
  stripPrefix = \case
    '"' : rest -> stripSuffix rest
    other -> stripSuffix other
  stripSuffix = \case
    '"' : [] -> []
    [] -> []
    x : xs -> x : stripSuffix xs

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
outputToRel (PI.Path p) = (</> PI.Path p) <$> asks output

-- | Prepend the source path.
-- 'm' can be 'Action' or 'Rules'.
sourceToRel :: (MonadReader Settings m) => P.Path SourceRel t -> m (P.Path P.Rel t)
sourceToRel (PI.Path p) = (</> PI.Path p) <$> asks source

mkOutputRelFile :: FilePath -> Q Exp
mkOutputRelFile = either (crashWith . show) (TH.lift . asOutputRel) . parseRelFile

outputRelFile :: QuasiQuoter
outputRelFile = qq mkOutputRelFile

mkOutputRelDir :: FilePath -> Q Exp
mkOutputRelDir = either (crashWith . show) (TH.lift . asOutputRel) . parseRelDir

outputRelDir :: QuasiQuoter
outputRelDir = qq mkOutputRelDir

mkSourceRelFile :: FilePath -> Q Exp
mkSourceRelFile = either (crashWith . show) (TH.lift . asSourceRel) . parseRelFile

sourceRelFile :: QuasiQuoter
sourceRelFile = qq mkSourceRelFile

mkSourceRelDir :: FilePath -> Q Exp
mkSourceRelDir = either (crashWith . show) (TH.lift . asSourceRel) . parseRelDir

sourceRelDir :: QuasiQuoter
sourceRelDir = qq mkSourceRelDir

qq :: (String -> Q Exp) -> QuasiQuoter
qq quoteExp' =
  QuasiQuoter
    { quoteExp = quoteExp'
    , quotePat = \_ ->
        crashWith "illegal QuasiQuote (allowed as expression only, used as a pattern)"
    , quoteType = \_ ->
        crashWith "illegal QuasiQuote (allowed as expression only, used as a type)"
    , quoteDec = \_ ->
        crashWith "illegal QuasiQuote (allowed as expression only, used as a declaration)"
    }
