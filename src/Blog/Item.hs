module Blog.Item
  ( Item (..)
  , MetadataError (..)
  , (<!>)
  , mkItem
  ) where

import Blog.Prelude

import qualified Chronos
import Control.Lens ((^?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Attoparsec.Text as AP
import qualified Text.Pandoc as Pandoc

data Item = Item
  { id :: Text
  , title :: Text
  , publish :: Date
  , tags :: [Text]
  , raw :: Aeson.Value
  , content :: [Pandoc.Block]
  }

data MetadataError
  = MissingKey FilePath String
  | MalformedPublishDate FilePath String
  | PostNotPublishedYet FilePath Text
  deriving stock (Show)

(<!>) :: Maybe b -> a -> Either a b
(<!>) mb err = maybe (Left err) Right mb
infixr 7 <!>

-- perhaps use something other than `Maybe` and upstream the error/reason?
-- I'd like to see "skipped because future", "could not parse date" as an error etc
mkItem
  :: Chronos.Day -> FilePath -> (Aeson.Value, [Pandoc.Block]) -> Either MetadataError Item
mkItem day p (v, content) = do
  id <- v ^? key "id" . _String <!> MissingKey p "id"
  title <- v ^? key "title" . _String <!> MissingKey p "title"
  pub <- v ^? key "publish" . _String <!> MissingKey p "publish"
  publish <- case AP.parseOnly (Chronos.parser_Dmy (Just '-')) pub of
    Left err -> Left $ MalformedPublishDate p err
    Right d -> pure d
  let
    tags = v ^.. key "tags" . values . _String
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
        raw = case v of
          Aeson.Object kv -> Aeson.Object $ KeyMap.insert "renderChangelog" renderChangelog' kv
          _ -> v
      in
        pure Item {..}
