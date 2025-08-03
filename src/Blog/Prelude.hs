module Blog.Prelude
  ( module P
  , identity
  , todo
  , parseJSON
  , crashWith
  ) where

import Chronos as P (Date)
import Control.Lens as P (to, traversed, (.~), (^.), (^..))
import Control.Monad as P (guard, join, when, (<=<), (=<<), (>=>))
import Control.Monad.Catch as P (MonadThrow)
import Control.Monad.IO.Class as P (MonadIO, liftIO)
import Control.Monad.Reader as P (ReaderT (ReaderT), ask, runReaderT)
import Control.Monad.Trans as P (lift)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens as P (key, values, _Array, _Bool, _String)
import Data.Either as P (lefts, rights)
import Data.Foldable as P (forM_, traverse_)
import Data.Function as P ((&))
import Data.Functor as P (void, ($>))
import Data.List as P (elem, filter, find, length, sortOn)
import Data.Maybe as P (catMaybes)
import Data.Ord as P (Down (..))
import Data.Text as P (Text)
import Data.Traversable as P (traverse)
import GHC.Generics as P (Generic)
import System.FilePath as P ((-<.>), (</>))
import Prelude (error)
import Prelude as P
  ( Bool (..)
  , Either (..)
  , Eq
  , FilePath
  , IO
  , Maybe (..)
  , Show
  , String
  , dropWhile
  , either
  , flip
  , fmap
  , fst
  , lookup
  , maybe
  , mconcat
  , mempty
  , not
  , null
  , otherwise
  , pure
  , putStrLn
  , reverse
  , sequence
  , show
  , snd
  , ($)
  , (&&)
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
  , (||)
  )

identity :: forall a. a -> a
identity x = x

todo :: a
todo = todo

parseJSON :: (Aeson.FromJSON a) => Aeson.Value -> Either String a
parseJSON v = case Aeson.fromJSON v of
  Aeson.Success a -> pure a
  Aeson.Error err -> Left err

crashWith :: forall a. String -> a
crashWith = error
