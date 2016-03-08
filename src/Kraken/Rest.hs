module Kraken.Rest where

import           Control.Monad.Trans.Either
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

type KrakenAPI     = APIVersion :> Services
type APIVersion    = "0"
type Public        = "public"
type Services      = TimeService
                     :<|>
                     AssetsService
type TimeService   = Public
                     :> "Time"
                     :> Get '[JSON] Time
type AssetsService = Public
                     :> "Assets"
                     :> ReqBody '[FormUrlEncoded] AssetsOptions
                     :> Post '[JSON] Assets

-----------------------------------------------------------------------------

api :: Proxy KrakenAPI
api = Proxy

-----------------------------------------------------------------------------

time       :: KrakenT Time
assets     :: AssetsOptions -> KrakenT Assets

time :<|> assets = client api (BaseUrl Https restHost restPort)
