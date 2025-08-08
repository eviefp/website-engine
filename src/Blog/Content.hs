module Blog.Content
  ( options
  , markdownToMetaAndContent
  , genenerateHtmlWithFixedWikiLinks
  , compileTemplate
  , substitute
  ) where

import Blog.Item
import qualified Blog.Path.Rel as Rel
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
import qualified Slick
import qualified Slick.Pandoc as Slick
import qualified Text.Mustache.Types as M
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as PW

compileTemplate :: Path Rel.Source File -> Action M.Template
compileTemplate = SP.liftAction . Slick.compileTemplate' . toFilePath <=< Rel.sourceToRel

substitute :: (Aeson.ToJSON a) => M.Template -> a -> Text
substitute template = Slick.substitute template . Aeson.toJSON

-- | Default Pandoc markdown reader options. Using other options may break some of the helpers.
options :: Pandoc.ReaderOptions
options =
  Slick.defaultMarkdownOptions
    { Pandoc.readerExtensions =
        mconcat
          [ Pandoc.extensionsFromList
              [ Pandoc.Ext_auto_identifiers -- todo: test
              , Pandoc.Ext_fenced_code_attributes -- todo: test
              , Pandoc.Ext_footnotes -- todo: test
              , Pandoc.Ext_wikilinks_title_after_pipe -- needed for obsidian links
              , Pandoc.Ext_yaml_metadata_block -- needed for metadata
              ]
          , Pandoc.githubMarkdownExtensions
          ]
    }

-- | Exported for testing. Should not be used.
markdownToMetaAndContent :: (MonadIO m) => Text -> m (Aeson.Value, [Pandoc.Block])
markdownToMetaAndContent content = do
  let
    writer = Pandoc.writeHtml5String Slick.defaultHtml5Options
  (Pandoc.Pandoc meta' blocks) <- unPandocM $ Pandoc.readMarkdown options content
  meta <- flattenMeta writer meta'
  pure (meta, blocks)

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

genenerateHtmlWithFixedWikiLinks
  :: (Partial, Traversable t)
  => [(ItemKind, [Item])]
  -> ItemKind
  -> t Pandoc.Block
  -> Action Aeson.Value
genenerateHtmlWithFixedWikiLinks cache def blocks = do
  let
    (result, logs) =
      runIdentity
        . flip State.runStateT []
        . Except.runExceptT
        $ PW.walkM (CL.transform cache def) (toList blocks)
  traverse_ printLog logs
  case result of
    Left _ -> crashWith "[Wikilinks] unrecoverable error. Stopping."
    Right fixedBlocks -> do
      let
        writer = Pandoc.writeHtml5String Slick.defaultHtml5Options
        doc = Pandoc.Pandoc mempty $ toList fixedBlocks
      outText <- unPandocM $ writer doc
      pure $ Aeson.String outText

unPandocM :: (MonadIO m) => Pandoc.PandocIO a -> m a
unPandocM p = do
  result <- liftIO $ Pandoc.runIO p
  either (crashWith . show) pure result
