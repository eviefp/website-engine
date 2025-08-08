module Blog.Path.Rel
  ( Output
  , Source
  , asOutput
  , asSource
  , outputToRel
  , sourceToRel
  , outputFile
  , outputDir
  , sourceFile
  , sourceDir
  , takeBaseName
  ) where

import Blog.Prelude
import Blog.Settings (Settings (..))

import Control.Monad.Reader.Class
import qualified Development.Shake.FilePath as Shake
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import qualified Language.Haskell.TH.Syntax as TH
import qualified Path.Internal.Posix as PI
import qualified Path.Posix as P

-- | Output-relative path.
data Output

-- | Source-relative path.
data Source

-- | This path should be assumed to be relative to the output.
-- Its internal representation/path does *NOT* include the output directory,
-- but will signal to 'Blog.Engine' functions to prepend it.
asOutput :: P.Path b t -> P.Path Output t
asOutput (PI.Path p) = PI.Path p

-- | This path should be assumed to be relative to the source.
-- Its internal representation/path does *NOT* include the source directory,
-- but will signal to 'Blog.Engine' functions to prepend it.
asSource :: P.Path b t -> P.Path Source t
asSource (PI.Path p) = PI.Path p

-- | Prepend the output path.
-- 'm' can be 'Action' or 'Rules'.
outputToRel :: (MonadReader Settings m) => P.Path Output t -> m (P.Path P.Rel t)
outputToRel (PI.Path p) = (</> PI.Path p) <$> asks output

-- | Prepend the source path.
-- 'm' can be 'Action' or 'Rules'.
sourceToRel :: (MonadReader Settings m) => P.Path Source t -> m (P.Path P.Rel t)
sourceToRel (PI.Path p) = (</> PI.Path p) <$> asks source

-- | Given a relative path, returns the base name.
-- The base name is the name of the file (so it strips any directories and file extension).
takeBaseName :: Path b File -> String
takeBaseName = Shake.takeBaseName . toFilePath

mkOutputFile :: FilePath -> Q Exp
mkOutputFile = either (crashWith . show) (TH.lift . asOutput) . parseRelFile

outputFile :: QuasiQuoter
outputFile = qq mkOutputFile

mkOutputDir :: FilePath -> Q Exp
mkOutputDir = either (crashWith . show) (TH.lift . asOutput) . parseRelDir

outputDir :: QuasiQuoter
outputDir = qq mkOutputDir

mkSourceFile :: FilePath -> Q Exp
mkSourceFile = either (crashWith . show) (TH.lift . asSource) . parseRelFile

sourceFile :: QuasiQuoter
sourceFile = qq mkSourceFile

mkSourceDir :: FilePath -> Q Exp
mkSourceDir = either (crashWith . show) (TH.lift . asSource) . parseRelDir

sourceDir :: QuasiQuoter
sourceDir = qq mkSourceDir

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
