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
import           Data.Text

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
type Services   = Time :<|> Assets
type Time       = Public :> "Time" :> Get '[JSON] Value
type Assets     = Public :> "Assets" :> ReqBody '[FormUrlEncoded] [(Text,Text)] :> Post '[JSON] Value

-----------------------------------------------------------------------------

api :: Proxy KrakenAPI
api = Proxy

-----------------------------------------------------------------------------

time       :: KrakenT Value
assets     :: [(Text,Text)] -> KrakenT Value

time :<|> assets = client api (BaseUrl Https restHost restPort)
