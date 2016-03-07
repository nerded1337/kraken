{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}

module Kraken.Rest where

import           Control.Monad.Trans.Either
import           Data.Aeson.Types
import           Data.Proxy
import           Servant.API
import           Servant.Client

import           Kraken.Types

-----------------------------------------------------------------------------

restHost :: Host
restHost = "api.kraken.com"

restPort :: Port
restPort = 443

-----------------------------------------------------------------------------

type KrakenT = EitherT ServantError IO

-----------------------------------------------------------------------------

runKraken :: KrakenT a -> IO (Either ServantError a)
runKraken = runEitherT

-----------------------------------------------------------------------------

type KrakenAPI  = APIVersion :> Services
type APIVersion = "0"
type Public     = "public"
type Services   = Public :> "Time" :> Get '[JSON] Value
                  :<|>
                  Public :> "Assets" :> Get '[JSON] Value

-----------------------------------------------------------------------------

api :: Proxy KrakenAPI
api = Proxy

-----------------------------------------------------------------------------

time       :: KrakenT Value
assets     :: KrakenT Value

time :<|> assets = client api (BaseUrl Https restHost restPort)
