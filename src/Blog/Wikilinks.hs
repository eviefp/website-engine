module Blog.Wikilinks
  ( transform
  , Log (..)
  ) where

import Blog.Item (Item, ItemId (..))
import qualified Blog.Item as Item
import Blog.Prelude
import Blog.Types

import Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import qualified Data.Text as T
import qualified Text.Pandoc as Pandoc

transform
  :: [(ItemKind, [Item])]
  -> ItemKind
  -> Pandoc.Inline
  -> ExceptT () (State [Log]) Pandoc.Inline
transform cache def =
  \case
    l@(Pandoc.Link attr@(_, classes, _) content@[Pandoc.Str originalText] target)
      | "wikilink" `elem` classes -> do
          (url, id, title) <- fixTarget cache def target
          appendState
            . Verbose
            $ "[WikiLinks] Original link "
            <> show l
          let
            fixed =
              if originalText == url || originalText == getItemId id
                then Pandoc.Link attr [Pandoc.Str title] (url, title)
                else Pandoc.Link attr content (url, title)
          appendState
            . Verbose
            $ "[WikiLinks] Fixed link "
            <> show fixed
          pure fixed
    link@Pandoc.Link {} -> do
      appendState
        . Verbose
        $ "[WikiLinks] Skipping link "
        <> show link
      pure link
    other -> pure other

fixTarget
  :: [(ItemKind, [Item])]
  -> ItemKind
  -> (Text, Text)
  -> ExceptT () (State [Log]) (Text, ItemId, Text)
fixTarget cache def (url, title) =
  case parseUrl url of
    Nothing -> do
      appendState
        . Warning
        $ "[WikiLinks] Warning: failed parse for "
        <> T.unpack url
        <> ". Wikilinks should only be used to link to internal content."
      pure (url, ItemId "", title)
    Just (k, itemId) ->
      case lookup k cache of
        Nothing -> do
          appendState
            . Error
            $ "[WikiLinks] Error: could not find key "
            <> show k
            <> " for url "
            <> T.unpack url
          Except.throwError ()
        Just items ->
          case find ((== itemId) . Item.id) items of
            Nothing -> do
              appendState
                . Error
                $ "[WikiLinks] Error: could not find itemId "
                <> show itemId
                <> " at key "
                <> show k
                <> " for url "
                <> T.unpack url
              Except.throwError ()
            Just item -> pure (url, Item.id item, Item.title item)
 where
  parseUrl :: Text -> Maybe (ItemKind, ItemId)
  parseUrl t =
    case dropWhile (== "") $ T.splitOn "/" t of
      [k, v] -> Just (ItemKind k, ItemId v)
      [v] -> Just (def, ItemId v)
      _ -> Nothing

appendState :: forall a m. (State.MonadState [a] m) => a -> m ()
appendState = State.modify' . flip (<>) . pure
