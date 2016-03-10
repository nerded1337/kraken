module Main where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BC (pack)
import Data.Text
import System.Environment
import System.Exit

import Kraken.Rest
import Kraken.Types
import Kraken.Util

-----------------------------------------------------------------------------

main :: IO ()
main = getConfig >>= either (const exitFailure) run 
  
run :: Config -> IO ()
run cfg = void $ runKraken cfg $ do
  io =<< time
  io =<< assets         (AssetOptions Currency [XXBT,XETH])
  io =<< assetPairs     (AssetPairOptions Info pairs)
  io =<< tickers        (TickerOptions pairs)
  io =<< ohlcs          (OHLCOptions xbtusd 60 Nothing)
  io =<< accountBalance
  io =<< tradeVolume

 where

  io :: Show a => a -> KrakenT ()
  io = liftIO . print

  pairs  = [xbtusd,xbteur]
  xbtusd = AssetPair XXBT ZUSD
  xbteur = AssetPair XXBT ZEUR

