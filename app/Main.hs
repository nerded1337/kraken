module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text

import Kraken.Types
import Kraken.Rest

-----------------------------------------------------------------------------

main :: IO ()
main = void $ runKraken $ do
  t <- time
  io t
  a <- assets (AssetsOptions Currency [XXBT,XETH])
  io a
 where
  io :: Show a => a -> KrakenT ()
  io = liftIO . print

