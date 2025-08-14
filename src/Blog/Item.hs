module Blog.Item
  ( Item (..)
  , ItemId (..)
  , TagName (..)
  , MetadataError (..)
  , (<!>)
  , mkItem
  , tagNameFromPath
  ) where

import qualified Blog.Path.Rel as RelPath
import Blog.Prelude

import qualified Chronos
import Control.Lens ((^?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Attoparsec.Text as AP
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Text.Pandoc as Pandoc

-- | Unique (per 'ItemKind') identifier for 'Item's.
newtype ItemId = ItemId
  { getItemId :: Text
  }
  deriving newtype (Show, Eq)

-- | The name for a tag — also acts as an identifier across 'ItemKind's.
newtype TagName = TagName
  { getTagName :: Text
  }
  deriving newtype (Eq, Aeson.ToJSON)

-- | Given a relative path as @foo/bar/some-file.html@, returns @TagName "some-file"@.
tagNameFromPath :: Path p File -> TagName
tagNameFromPath = TagName . T.pack . RelPath.takeBaseName

-- | Holds data about an item/post.
-- All the metadata fields (id, title, etc.) are also present in 'metadata' as raw JSON.
data Item = Item
  { id :: ItemId
  , title :: Text
  , publish :: Date
  , tags :: [TagName]
  , metadata :: Aeson.Value
  -- ^ yaml metadata from the markdown files; contains all of the above fields as well
  -- all the fields are parsed as markdown and will be converted to html!
  , documentContent :: [Pandoc.Block]
  -- ^ pandoc-representation of the markdown content
  }

-- | Represents an error in parsing the metadata.
data MetadataError
  = MissingKey (Path Rel File) String
  | MalformedPublishDate (Path Rel File) String
  | PostNotPublishedYet (Path Rel File) ItemId
  deriving stock (Show)

-- | Add an error 'note' to a 'Maybe', in case it is 'Nothing'.
(<!>) :: Maybe b -> a -> Either a b
(<!>) mb err = maybe (Left err) Right mb

infixr 7 <!>

dateToISO8601 :: Chronos.Date -> Aeson.Value
dateToISO8601 = Aeson.String . TL.toStrict . TB.toLazyText . Chronos.builder_Ymd (Just '-')

-- | Parse an 'Item' out of a markdown file.
mkItem :: Chronos.Day -> Path Rel File -> (Aeson.Value, [Pandoc.Block]) -> Either MetadataError Item
mkItem day p (v, documentContent) = do
  id <- v ^? key "id" . _String . to ItemId <!> MissingKey p "id"
  title <- v ^? key "title" . _String <!> MissingKey p "title"
  pub <- v ^? key "publish" . _String <!> MissingKey p "publish"
  publish <- case AP.parseOnly (Chronos.parser_Dmy $ Just '-') pub of
    Left err -> Left $ MalformedPublishDate p err
    Right d -> pure d
  let
    tags = v ^.. key "tags" . values . _String . to TagName
  if Chronos.dayToDate day < publish
    then Left $ PostNotPublishedYet p id
    else
      let
        changelog = v ^? key "changelog" . _Array
        -- if `renderChangelog` is present, return that value
        -- if not, if `changelog` is not present, return False
        -- else, return whether the changelog is not empty
        renderChangelog = maybe (maybe False (not . null) changelog) identity $ v ^? key "renderChangelog" . _Bool
        renderChangelog' = Aeson.Bool . maybe renderChangelog ((&& renderChangelog) . not . null) $ changelog
        isoPublishDate = dateToISO8601 publish
        metadata = case v of
          Aeson.Object kv ->
            Aeson.Object
              $ KeyMap.insert "renderChangelog" renderChangelog'
              $ KeyMap.insert "isoPublishDate" isoPublishDate kv
          _ -> v
      in
        pure Item {..}
