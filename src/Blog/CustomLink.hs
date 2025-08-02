module Blog.CustomLink
  ( transform
  ) where

import Blog.Item (Item)
import qualified Blog.Item as Item
import Blog.Prelude

import qualified Data.Text as T
import qualified Debug.Trace as D
import qualified Text.Pandoc as Pandoc

transform :: [(Text, [Item])] -> Text -> Pandoc.Inline -> Pandoc.Inline
transform cache def =
  \case
    Pandoc.Link attr@("", ["wikilink"], []) _ target ->
      let
        (url, title) = fixTarget cache def target
      in
        Pandoc.Link attr [Pandoc.Str title] (url, title)
    other -> other

fixTarget :: [(Text, [Item])] -> Text -> (Text, Text) -> (Text, Text)
fixTarget cache def (url, title) =
  case parseUrl url of
    Nothing -> D.trace ("[WikiLinks] failed parse for " <> T.unpack url) (url, title)
    Just (k, itemId) ->
      case find ((== k) . fst) cache of
        Nothing -> D.trace ("could not find key " <> T.unpack k) (url, title)
        Just (_, items) ->
          case find ((== itemId) . Item.id) items of
            Nothing -> D.trace ("could not find itemId" <> T.unpack itemId) (url, title)
            Just item ->
              (url, Item.title item)
 where
  parseUrl :: Text -> Maybe (Text, Text)
  parseUrl t =
    case dropWhile (== "") $ T.splitOn "/" t of
      [k, v] -> Just (k, v)
      [v] -> Just (def, v)
      _ -> Nothing
