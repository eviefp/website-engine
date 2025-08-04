module Blog.Wikilinks
  ( transform
  ) where

import Blog.Item (Item)
import qualified Blog.Item as Item
import Blog.Prelude

import qualified Data.Text as T
import qualified Development.Shake as Shake
import qualified Text.Pandoc as Pandoc

transform :: [(Text, [Item])] -> Text -> Pandoc.Inline -> Shake.Action Pandoc.Inline
transform cache def =
  \case
    l@(Pandoc.Link attr@(_, classes, _) content@[Pandoc.Str originalText] target)
      | "wikilink" `elem` classes -> do
          (url, id, title) <- fixTarget cache def target
          Shake.putVerbose $ "[WikiLinks] Original link " <> show l
          let
            fixed =
              if originalText == url || originalText == id
                then Pandoc.Link attr [Pandoc.Str title] (url, title)
                else Pandoc.Link attr content (url, title)
          Shake.putVerbose $ "[WikiLinks] Fixed link " <> show fixed
          pure fixed
    link@Pandoc.Link {} -> do
      Shake.putVerbose $ "[WikiLinks] Skipping link " <> show link
      pure link
    other -> pure other

fixTarget :: [(Text, [Item])] -> Text -> (Text, Text) -> Shake.Action (Text, Text, Text)
fixTarget cache def (url, title) =
  case parseUrl url of
    Nothing -> do
      Shake.putWarn
        $ "[WikiLinks] Warning: failed parse for "
        <> T.unpack url
        <> ". Wikilinks should only be used to link to internal content."
      pure (url, "", title)
    Just (k, itemId) ->
      case find ((== k) . fst) cache of
        Nothing -> do
          Shake.putError
            $ "[WikiLinks] Error: could not find key "
            <> T.unpack k
            <> " for url "
            <> T.unpack url
          crashWith "WikiLinks unrecoverable error; please fix the link."
        Just (_, items) ->
          case find ((== itemId) . Item.id) items of
            Nothing -> do
              Shake.putError
                $ "[WikiLinks] Error: could not find itemId "
                <> T.unpack itemId
                <> " at key "
                <> T.unpack k
                <> " for url "
                <> T.unpack url
              crashWith "WikiLinks unrecoverable error; please fix the link."
            Just item -> pure (url, Item.id item, Item.title item)
 where
  parseUrl :: Text -> Maybe (Text, Text)
  parseUrl t =
    case dropWhile (== "") $ T.splitOn "/" t of
      [k, v] -> Just (k, v)
      [v] -> Just (def, v)
      _ -> Nothing
