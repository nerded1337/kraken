{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text

import Kraken.Types
import Kraken.Rest

-----------------------------------------------------------------------------

main :: IO ()
main = void $ runKraken $ do
  io =<< time
  io =<< assets (AssetsOptions Currency [XXBT,XETH])
 where
  io = liftIO . print

