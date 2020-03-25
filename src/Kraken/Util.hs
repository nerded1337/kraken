{-# LANGUAGE TypeApplications #-}
module Kraken.Util where

import           Kraken.Types (Config)
import           System.Envy  (decodeEnv)

-----------------------------------------------------------------------------

getConfig :: IO (Either String Config)
getConfig = decodeEnv @Config

