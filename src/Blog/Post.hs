module Blog.Post
  ( buildPost
  ) where

import qualified Blog.Config as Config
import Blog.Prelude
import qualified Data.Text as T
import qualified Development.Shake as Shake
import qualified Development.Shake.FilePath as Shake
import Development.Shake.FilePath ((-<.>))
import qualified Slick
import qualified Slick.Pandoc as Slick
import qualified Text.Pandoc as Pandoc
import Data.Aeson (Value)

--data Post = Post
--  { id :: Text
--  , content :: Text
--  , publish :: Date
--  }

buildPost :: Shake.Action ()
buildPost =
  void $ flip Shake.forP buildPostPage =<< Shake.getDirectoryFiles "." ["site/post//*.md"]

buildPostPage :: FilePath -> Shake.Action ()
buildPostPage path = do
  post <-
    Slick.markdownToHTMLWithOpts options Slick.defaultHtml5Options
      . T.pack
      =<< Shake.readFile' path
  let
    url = T.pack . Shake.dropDirectory1 $ path -<.> "html"
  liftIO $ putStrLn $ show post
  template <- Slick.compileTemplate' "site/template/post.html"
  Shake.writeFile' (Config.output </> T.unpack url)
    . T.unpack
    . Slick.substitute template
    . Config.withMetadataObject "post"
    . fixDate
    $ post
 where
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

  fixDate :: Value -> Value
  fixDate = identity
