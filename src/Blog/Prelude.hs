module Blog.Prelude
  ( module P
  , identity
  , todo
  , parseJSON
  ) where

import Chronos as P (Date)
import Control.Lens as P (to, traversed, (.~), (^.), (^..))
import Control.Monad as P (guard, when, (<=<), (=<<), (>=>))
import Control.Monad.Catch as P (MonadThrow)
import Control.Monad.IO.Class as P (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens as P (key, _String)
import Data.Function as P ((&))
import Data.Functor as P (void)
import Data.List as P (find, length, sortOn)
import Data.Maybe as P (catMaybes)
import Data.Ord as P (Down (..))
import Data.Text as P (Text)
import GHC.Generics as P (Generic)
import System.FilePath as P ((</>))
import Prelude as P
  ( Bool (..)
  , Either (..)
  , Eq
  , FilePath
  , IO
  , Maybe (..)
  , Show
  , String
  , either
  , fail
  , flip
  , fmap
  , fst
  , maybe
  , mconcat
  , not
  , null
  , otherwise
  , pure
  , putStrLn
  , reverse
  , show
  , snd
  , ($)
  , (*>)
  , (.)
  , (<)
  , (<$>)
  , (<*>)
  , (<=)
  , (<>)
  , (==)
  , (>)
  , (>=)
  , (>>=)
  )

identity :: forall a. a -> a
identity x = x

todo :: a
todo = todo

parseJSON :: (Aeson.FromJSON a) => Aeson.Value -> Either String a
parseJSON v = case Aeson.fromJSON v of
  Aeson.Success a -> pure a
  Aeson.Error err -> Left err
