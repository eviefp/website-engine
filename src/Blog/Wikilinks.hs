module Blog.Wikilinks
  ( transform
  , Log (..)
  ) where

import Blog.Item (Item)
import qualified Blog.Item as Item
import Blog.Prelude

import Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import qualified Data.Text as T
import qualified Text.Pandoc as Pandoc

data Log
  = Verbose String
  | Warning String
  | Error String
  deriving stock (Show)

transform :: [(Text, [Item])] -> Text -> Pandoc.Inline -> ExceptT () (State [Log]) (Pandoc.Inline)
transform cache def =
  \case
    l@(Pandoc.Link attr@(_, classes, _) content@[Pandoc.Str originalText] target)
      | "wikilink" `elem` classes -> do
          (url, id, title) <- fixTarget cache def target
          State.modify'
            . flip (<>)
            . pure
            . Verbose
            $ "[WikiLinks] Original link "
            <> show l
          let
            fixed =
              if originalText == url || originalText == id
                then Pandoc.Link attr [Pandoc.Str title] (url, title)
                else Pandoc.Link attr content (url, title)
          State.modify'
            . flip (<>)
            . pure
            . Verbose
            $ "[WikiLinks] Fixed link "
            <> show fixed
          pure fixed
    link@Pandoc.Link {} -> do
      State.modify'
        . flip (<>)
        . pure
        . Verbose
        $ "[WikiLinks] Skipping link "
        <> show link
      pure link
    other -> pure other

fixTarget
  :: [(Text, [Item])] -> Text -> (Text, Text) -> ExceptT () (State [Log]) ((Text, Text, Text))
fixTarget cache def (url, title) =
  case parseUrl url of
    Nothing -> do
      State.modify'
        . flip (<>)
        . pure
        . Warning
        $ "[WikiLinks] Warning: failed parse for "
        <> T.unpack url
        <> ". Wikilinks should only be used to link to internal content."
      pure $ (url, "", title)
    Just (k, itemId) ->
      case find ((== k) . fst) cache of
        Nothing -> do
          State.modify'
            . flip (<>)
            . pure
            . Error
            $ "[WikiLinks] Error: could not find key "
            <> T.unpack k
            <> " for url "
            <> T.unpack url
          Except.throwError ()
        Just (_, items) ->
          case find ((== itemId) . Item.id) items of
            Nothing -> do
              State.modify'
                . flip (<>)
                . pure
                . Error
                $ "[WikiLinks] Error: could not find itemId "
                <> T.unpack itemId
                <> " at key "
                <> T.unpack k
                <> " for url "
                <> T.unpack url
              Except.throwError ()
            Just item -> pure (url, Item.id item, Item.title item)
 where
  parseUrl :: Text -> Maybe (Text, Text)
  parseUrl t =
    case dropWhile (== "") $ T.splitOn "/" t of
      [k, v] -> Just (k, v)
      [v] -> Just (def, v)
      _ -> Nothing
