{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text

import Kraken.Rest

-----------------------------------------------------------------------------

main :: IO ()
main = void $ runKraken $ do
  io =<< time
  io =<< assets [("asset","XXBT,XETH"),("aclass","currency")]
 where
  io = liftIO . print

