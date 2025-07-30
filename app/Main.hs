module Main where

import qualified Blog (runNew)
import Prelude

main :: IO ()
main = Blog.runNew
