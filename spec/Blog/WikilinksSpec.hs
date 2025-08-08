{-# OPTIONS_GHC -Wno-orphans #-}

module Blog.WikilinksSpec
  ( spec
  ) where

import qualified Blog.Content as Content
import Blog.Item (Item (..), ItemId (ItemId))
import Blog.Prelude
import Blog.Wikilinks (Log (..), transform)

import qualified Chronos
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Aeson as Aeson
import Data.Functor.Identity (runIdentity)
import Data.List (unlines)
import qualified Data.Text as T
import qualified Text.Pandoc as Pandoc

import Blog.Types (ItemKind)
import Test.Hspec
import qualified Test.Hspec.Golden as Golden

spec :: Spec
spec = do
  context "Wikilinks" do
    context "pandoc regression tests -- parsing links" do
      traverse_
        goldenMarkdownToMetaAndContent
        [ "[link]"
        , "[title](link)"
        , "[[link]]"
        , "[[link|title]]"
        ]

    context "transforming links" do
      traverse_
        goldenTransformLink
        [ TransformLinkSpec
            { testName = "[[page-1]] transforms correctly"
            , itemKey = "page"
            , attr = ["wikilink"]
            , content = "page-1"
            , url = "page-1"
            , title = ""
            }
        , TransformLinkSpec
            { testName = "does not override custom names"
            , itemKey = "page"
            , attr = ["wikilink"]
            , content = "some page name"
            , url = "page-1"
            , title = ""
            }
        , TransformLinkSpec
            { testName = "cross-group links"
            , itemKey = "page"
            , attr = ["wikilink"]
            , content = "wiki-1"
            , url = "/wiki/wiki-1"
            , title = ""
            }
        , TransformLinkSpec
            { testName = "does not override names even if they match another item's id"
            , itemKey = "page"
            , attr = ["wikilink"]
            , content = "wiki-2"
            , url = "/wiki/wiki-1"
            , title = ""
            }
        , TransformLinkSpec
            { testName = "does not find items under the wrong key"
            , itemKey = "page"
            , attr = ["wikilink"]
            , content = "wiki-1"
            , url = "/page/wiki-1"
            , title = ""
            }
        ]

goldenMarkdownToMetaAndContent :: String -> Spec
goldenMarkdownToMetaAndContent s =
  Golden.golden ("goldenMarkdownToMetaAndContent-" <> s)
    . fmap show
    . Content.markdownToMetaAndContent
    . T.pack
    $ s

data TransformLinkSpec = TransformLinkSpec
  { testName :: String
  , itemKey :: ItemKind
  , attr :: [Text]
  , content :: String
  , url :: Text
  , title :: Text
  }

goldenTransformLink :: TransformLinkSpec -> Spec
goldenTransformLink TransformLinkSpec {..} =
  Golden.golden testName do
    today <- liftIO Chronos.today
    let
      (result, logs) =
        runIdentity
          . flip State.runStateT []
          . Except.runExceptT
          . transform (sampleCache today) itemKey
          $ Pandoc.Link ("", attr, []) [Pandoc.Str $ T.pack content] (url, title)
    pure $ show result <> "\n" <> unlines (show <$> logs)

sampleCache :: Chronos.Day -> [(ItemKind, [Item])]
sampleCache today =
  [
    ( "page"
    ,
      [ Item
          (ItemId "page-1")
          "page-1-title"
          (Chronos.dayToDate today)
          []
          Aeson.Null
          []
      , Item
          (ItemId "page-2")
          "page-2-title"
          (Chronos.dayToDate today)
          []
          Aeson.Null
          []
      ]
    )
  ,
    ( "wiki"
    ,
      [ Item
          (ItemId "wiki-1")
          "wiki-1-title"
          (Chronos.dayToDate today)
          []
          Aeson.Null
          [Pandoc.Para [Pandoc.Link ("", ["wikilinks"], []) [Pandoc.Str "wiki-2"] ("/wiki/wiki-2", "")]]
      , Item
          (ItemId "wiki-2")
          "wiki-2-title"
          (Chronos.dayToDate today)
          []
          Aeson.Null
          [Pandoc.Para [Pandoc.Link ("", ["wikilinks"], []) [Pandoc.Str "wiki-1"] ("wiki-1", "")]]
      ]
    )
  ]

deriving instance Show Log
