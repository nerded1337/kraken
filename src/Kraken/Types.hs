{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kraken.Types where

import           Servant.API
import           Data.Text (intercalate,pack,toLower)

-----------------------------------------------------------------------------

type Host = String
type Port = Int

-----------------------------------------------------------------------------

data Class =
    Currency
    deriving Show

instance ToText Class where
  toText = toLower . pack . show

-----------------------------------------------------------------------------

data Asset = 
    XXBT
  | XETH
    deriving Show

instance ToText Asset where
  toText = pack . show

-----------------------------------------------------------------------------

data AssetsOptions = AssetsOptions
  { assetsoptionsClass :: Class
  , assetsoptionsAssets :: [Asset]
  } deriving Show

instance ToFormUrlEncoded AssetsOptions where
  toFormUrlEncoded AssetsOptions{..} =
    [ ("aclass",toText assetsoptionsClass)
    , ("asset",(intercalate "," . map toText) assetsoptionsAssets)
    ]