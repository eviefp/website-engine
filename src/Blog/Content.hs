module Blog.Content
  ( compileTemplate
  , substitute
  , markdownToMetaAndContent
  , generateHtmlWithFixedWikiLinks
  ) where

import Blog.Core
import Blog.Item
import qualified Blog.Path.Rel as RelPath
import Blog.Prelude
import Blog.Types
import qualified Blog.Wikilinks as CL

import Control.Exception.Extra (Partial)
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Aeson as Aeson
import Data.Functor.Identity (runIdentity)
import qualified Data.Text as T
import qualified Development.Shake.Plus as SP
import qualified Text.Mustache as M
import qualified Text.Mustache.Compile as M
import Text.Pandoc (def)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Highlighting as Pandoc
import qualified Text.Pandoc.Walk as PW

-- | Given the path to a template, compile it.
-- See 'substitute' for how to use it.
compileTemplate :: Path RelPath.Source File -> Action M.Template
compileTemplate templatePath = do
  relPath <- RelPath.sourceToRel templatePath
  SP.needP [relPath]
  liftIO (M.localAutomaticCompile $ toFilePath relPath) >>= \case
    Right template -> do
      SP.need (M.getPartials . M.ast $ template)
      pure template
    Left err -> putError $ "[compileTemplate] error" <> show err

-- | Using a compiled template, substitute in the json data.
-- See 'compileTemplate'.
substitute :: (Aeson.ToJSON a) => M.Template -> a -> Text
substitute template = M.substitute template . Aeson.toJSON

-- | Exported for testing. Should not be used.
markdownToMetaAndContent :: (MonadIO m) => Text -> m (Aeson.Value, [Pandoc.Block])
markdownToMetaAndContent content = do
  let
    writer = Pandoc.writeHtml5String htmlOptions
  (Pandoc.Pandoc meta' blocks) <- unPandocM $ Pandoc.readMarkdown options content
  meta <- flattenMeta writer meta'
  pure (meta, blocks)

generateHtmlWithFixedWikiLinks
  :: (Partial, Traversable t)
  => [(ItemKind, [Item])]
  -> ItemKind
  -> t Pandoc.Block
  -> Action Aeson.Value
generateHtmlWithFixedWikiLinks cache kind blocks = do
  let
    (result, logs) =
      runIdentity
        . flip State.runStateT []
        . Except.runExceptT
        $ PW.walkM (CL.transform cache kind) (toList blocks)
  traverse_ printLog logs
  case result of
    Left _ -> crashWith "[Wikilinks] unrecoverable error. Stopping."
    Right fixedBlocks -> do
      let
        writer = Pandoc.writeHtml5String htmlOptions
        doc = Pandoc.Pandoc mempty $ toList fixedBlocks
      outText <- unPandocM $ writer doc
      pure $ Aeson.String outText

flattenMeta
  :: (MonadIO m)
  => (Pandoc.Pandoc -> Pandoc.PandocIO T.Text)
  -> Pandoc.Meta
  -> m Aeson.Value
flattenMeta writer (Pandoc.Meta meta) = Aeson.toJSON <$> traverse go meta
 where
  go :: (MonadIO m) => Pandoc.MetaValue -> m Aeson.Value
  go (Pandoc.MetaMap m) = Aeson.toJSON <$> traverse go m
  go (Pandoc.MetaList m) = Aeson.toJSONList <$> traverse go m
  go (Pandoc.MetaBool m) = pure $ Aeson.toJSON m
  go (Pandoc.MetaString m) = pure $ Aeson.toJSON m
  go (Pandoc.MetaInlines m) = Aeson.toJSON <$> (unPandocM . writer . Pandoc.Pandoc mempty . (: []) . Pandoc.Plain $ m)
  go (Pandoc.MetaBlocks m) = Aeson.toJSON <$> (unPandocM . writer . Pandoc.Pandoc mempty $ m)

unPandocM :: (MonadIO m) => Pandoc.PandocIO a -> m a
unPandocM p = do
  result <- liftIO $ Pandoc.runIO p
  either (crashWith . show) pure result

options :: Pandoc.ReaderOptions
options =
  def
    { Pandoc.readerExtensions =
        mconcat
          [ Pandoc.extensionsFromList
              [ Pandoc.Ext_auto_identifiers
              , Pandoc.Ext_fenced_code_attributes
              , Pandoc.Ext_footnotes
              , Pandoc.Ext_wikilinks_title_after_pipe
              , Pandoc.Ext_yaml_metadata_block
              ]
          , Pandoc.githubMarkdownExtensions
          ]
    }

htmlOptions :: Pandoc.WriterOptions
htmlOptions =
  def
    { Pandoc.writerHighlightStyle = Just Pandoc.tango
    }
