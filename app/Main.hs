module Main where

import Control.Monad
import Control.Monad.IO.Class

import Kraken.Rest

-----------------------------------------------------------------------------

main :: IO ()
main = void $ runKraken $ do
  io =<< time
  io =<< assets
 where
  io = liftIO . print

