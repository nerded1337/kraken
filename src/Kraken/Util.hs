module Kraken.Util where

import qualified Data.ByteString.Char8 as BC
import           System.Environment

import           Kraken.Types

-----------------------------------------------------------------------------

getConfig :: IO (Either String Config)
getConfig = do
  Just apiKey     <- lookupEnv "KRAKEN_API_KEY"
  Just apiPrivKey <- lookupEnv "KRAKEN_API_PRIVKEY"
  apiPass         <- lookupEnv "KRAKEN_API_PASSWORD"
  return $ mkConfig (BC.pack apiKey)
                    (BC.pack apiPrivKey)
                    (fmap BC.pack apiPass)