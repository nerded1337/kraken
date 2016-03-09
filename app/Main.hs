module Main where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BC (pack)
import Data.Text
import System.Environment

import Kraken.Types
import Kraken.Rest

-----------------------------------------------------------------------------

main :: IO ()
main = do
  
  Just apiKey     <- lookupEnv "KRAKEN_API_KEY"
  Just apiPrivKey <- lookupEnv "KRAKEN_API_PRIVKEY"
  apiPass         <- lookupEnv "KRAKEN_API_PASSWORD"
  
  let Right cfg = mkConfig (BC.pack apiKey)
                           (BC.pack apiPrivKey)
                           (fmap BC.pack apiPass)
  
  void $ runKraken cfg $ do
    io =<< time
    io =<< assets (AssetOptions Currency [XXBT,XETH])
    io =<< assetPairs ()
    io =<< accountBalance
    io =<< tradeVolume

 where

  io :: Show a => a -> KrakenT ()
  io = liftIO . print
