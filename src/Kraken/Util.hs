module Kraken.Util where

import           Data.Maybe
import           System.Envy

import           Kraken.Types

-----------------------------------------------------------------------------

getConfig :: IO (Either String Config)
getConfig = decodeEnv >>= return . either Left config
 where
  config :: EnvVars -> Either String Config
  config EnvVars{..} = mkConfig
    (fromMaybe "" envvarsAPIKey)
    (fromMaybe "" envvarsPrivateKeyB64)
    envvarsPassword

