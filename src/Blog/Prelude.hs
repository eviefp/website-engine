module Blog.Prelude
  ( module P
  , identity
  , todo
  ) where

import Chronos as P (Date)
import Control.Lens as P (to, traversed, (.~), (^.), (^..))
import Control.Monad as P (guard, when, (<=<), (=<<))
import Control.Monad.Catch as P (MonadThrow)
import Control.Monad.IO.Class as P (MonadIO, liftIO)
import Data.Function as P ((&))
import Data.Functor as P (void)
import Data.List as P (sortOn)
import Data.Maybe as P (catMaybes)
import Data.Ord as P (Down (..))
import Data.Text as P (Text)
import GHC.Generics as P (Generic)
import System.FilePath as P ((</>))
import Prelude as P
  ( Bool (..)
  , Either (..)
  , FilePath
  , IO
  , Maybe (..)
  , String
  , either
  , fail
  , flip
  , fmap
  , maybe
  , mconcat
  , otherwise
  , pure
  , putStrLn
  , reverse
  , show
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
