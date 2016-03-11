module Kraken.Rest where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Crypto.Hash
import           Data.Aeson.Types
import           Data.Byteable
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid
import           Data.Proxy
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Servant.API
import           Servant.Client

import           Kraken.Types

-----------------------------------------------------------------------------

restHost :: Host
restHost = "api.kraken.com"

restPort :: Port
restPort = 443

-----------------------------------------------------------------------------

type ServantT = EitherT ServantError IO
type KrakenT  = ReaderT Config ServantT

runKraken :: Config -> KrakenT a -> IO (Either ServantError a)
runKraken cfg = runEitherT . flip runReaderT cfg

-----------------------------------------------------------------------------

type KrakenAPI             = TimeService
                             :<|> AssetService
                             :<|> AssetPairService
                             :<|> TickerService
                             :<|> OHLCService
                             :<|> OrderBookService
                             :<|> TradeService
                             :<|> SpreadService
                             :<|> AccountBalanceService
                             :<|> TradeBalanceService
                             :<|> OpenOrdersService
                             :<|> ClosedOrdersService
                             :<|> QueryOrdersService
                             :<|> TradeHistoryService
                             :<|> QueryTradesService
                             :<|> OpenPositionsService
                             :<|> LedgersService
                             :<|> QueryLedgersService
                             :<|> TradeVolumeService

type TimeService           = APIVersion
                             :> Public
                             :> "Time"
                             :> Get '[JSON] Time
type AssetService          = APIVersion
                             :> Public
                             :> "Assets"
                             :> ReqBody '[FormUrlEncoded] AssetOptions
                             :> Post '[JSON] Assets
type AssetPairService      = APIVersion
                             :> Public
                             :> "AssetPairs"
                             :> ReqBody '[FormUrlEncoded] AssetPairOptions
                             :> Post '[JSON] AssetPairs
type TickerService         = APIVersion
                             :> Public
                             :> "Ticker"
                             :> ReqBody '[FormUrlEncoded] TickerOptions
                             :> Post '[JSON] Tickers
type OHLCService           = APIVersion
                             :> Public
                             :> "OHLC"
                             :> ReqBody '[FormUrlEncoded] OHLCOptions
                             :> Post '[JSON] OHLCs
type OrderBookService      = APIVersion
                             :> Public
                             :> "Depth"
                             :> ReqBody '[FormUrlEncoded] OrderBookOptions
                             :> Post '[JSON] OrderBook
type TradeService          = APIVersion
                             :> Public
                             :> "Trades"
                             :> ReqBody '[FormUrlEncoded] TradeOptions
                             :> Post '[JSON] Trades
type SpreadService         = APIVersion
                             :> Public
                             :> "Spread"
                             :> ReqBody '[FormUrlEncoded] SpreadOptions
                             :> Post '[JSON] Spreads
type AccountBalanceService = APIVersion
                             :> Private
                             :> "Balance"
                             :> Header "API-Key" Text
                             :> Header "API-Sign" Text
                             :> ReqBody '[FormUrlEncoded] (PrivReq ())
                             :> Post '[JSON] Value
type TradeBalanceService   = APIVersion
                             :> Private
                             :> "TradeBalance"
                             :> Header "API-Key" Text
                             :> Header "API-Sign" Text
                             :> ReqBody '[FormUrlEncoded] (PrivReq TradeBalanceOptions)
                             :> Post '[JSON] Value
type OpenOrdersService     = APIVersion
                             :> Private
                             :> "OpenOrders"
                             :> Header "API-Key" Text
                             :> Header "API-Sign" Text
                             :> ReqBody '[FormUrlEncoded] (PrivReq OpenOrdersOptions)
                             :> Post '[JSON] Value
type ClosedOrdersService   = APIVersion
                             :> Private
                             :> "ClosedOrders"
                             :> Header "API-Key" Text
                             :> Header "API-Sign" Text
                             :> ReqBody '[FormUrlEncoded] (PrivReq ())
                             :> Post '[JSON] Value
type QueryOrdersService    = APIVersion
                             :> Private
                             :> "QueryOrders"
                             :> Header "API-Key" Text
                             :> Header "API-Sign" Text
                             :> ReqBody '[FormUrlEncoded] (PrivReq ())
                             :> Post '[JSON] Value
type TradeHistoryService   = APIVersion
                             :> Private
                             :> "TradeHistory"
                             :> Header "API-Key" Text
                             :> Header "API-Sign" Text
                             :> ReqBody '[FormUrlEncoded] (PrivReq ())
                             :> Post '[JSON] Value
type QueryTradesService    = APIVersion
                             :> Private
                             :> "QueryTrades"
                             :> Header "API-Key" Text
                             :> Header "API-Sign" Text
                             :> ReqBody '[FormUrlEncoded] (PrivReq ())
                             :> Post '[JSON] Value
type OpenPositionsService  = APIVersion
                             :> Private
                             :> "OpenPositions"
                             :> Header "API-Key" Text
                             :> Header "API-Sign" Text
                             :> ReqBody '[FormUrlEncoded] (PrivReq ())
                             :> Post '[JSON] Value
type LedgersService        = APIVersion
                             :> Private
                             :> "Ledgers"
                             :> Header "API-Key" Text
                             :> Header "API-Sign" Text
                             :> ReqBody '[FormUrlEncoded] (PrivReq ())
                             :> Post '[JSON] Value
type QueryLedgersService   = APIVersion
                             :> Private
                             :> "QueryLedgers"
                             :> Header "API-Key" Text
                             :> Header "API-Sign" Text
                             :> ReqBody '[FormUrlEncoded] (PrivReq ())
                             :> Post '[JSON] Value
type TradeVolumeService    = APIVersion
                             :> Private
                             :> "TradeVolume"
                             :> Header "API-Key" Text
                             :> Header "API-Sign" Text
                             :> ReqBody '[FormUrlEncoded] (PrivReq TradeVolumeOptions)
                             :> Post '[JSON] Value
  
type APIVersion            = "0"
type Public                = "public"
type Private               = "private"

-----------------------------------------------------------------------------

api :: Proxy KrakenAPI
api = Proxy

-----------------------------------------------------------------------------

time_           :: ServantT Time
assets_         :: AssetOptions -> ServantT Assets
assetPairs_     :: AssetPairOptions -> ServantT AssetPairs
tickers_        :: TickerOptions -> ServantT Tickers
ohlcs_          :: OHLCOptions -> ServantT OHLCs
orderBook_      :: OrderBookOptions -> ServantT OrderBook
trades_         :: TradeOptions -> ServantT Trades
spreads_        :: SpreadOptions -> ServantT Spreads
accountBalance_ :: Maybe Text -> Maybe Text -> PrivReq () -> ServantT Value
tradeBalance_   :: Maybe Text -> Maybe Text -> PrivReq TradeBalanceOptions -> ServantT Value
openOrders_     :: Maybe Text -> Maybe Text -> PrivReq OpenOrdersOptions -> ServantT Value
closedOrders_   :: Maybe Text -> Maybe Text -> PrivReq () -> ServantT Value
queryOrders_    :: Maybe Text -> Maybe Text -> PrivReq () -> ServantT Value
tradeHistory_   :: Maybe Text -> Maybe Text -> PrivReq () -> ServantT Value
queryTrades_    :: Maybe Text -> Maybe Text -> PrivReq () -> ServantT Value
openPositions_  :: Maybe Text -> Maybe Text -> PrivReq () -> ServantT Value
ledgers_        :: Maybe Text -> Maybe Text -> PrivReq () -> ServantT Value
queryLedgers_   :: Maybe Text -> Maybe Text -> PrivReq () -> ServantT Value
tradeVolume_    :: Maybe Text -> Maybe Text -> PrivReq TradeVolumeOptions -> ServantT Value

time_
  :<|> assets_
  :<|> assetPairs_
  :<|> tickers_
  :<|> ohlcs_
  :<|> orderBook_
  :<|> trades_
  :<|> spreads_
  :<|> accountBalance_ 
  :<|> tradeBalance_
  :<|> openOrders_
  :<|> closedOrders_
  :<|> queryOrders_
  :<|> tradeHistory_
  :<|> queryTrades_
  :<|> openPositions_
  :<|> ledgers_
  :<|> queryLedgers_
  :<|> tradeVolume_  = client api (BaseUrl Https restHost restPort)

-----------------------------------------------------------------------------

time :: KrakenT Time
time = lift time_

assets :: AssetOptions -> KrakenT Assets
assets = lift . assets_

assetPairs :: AssetPairOptions -> KrakenT AssetPairs
assetPairs = lift . assetPairs_

tickers :: TickerOptions -> KrakenT Tickers
tickers = lift . tickers_

ohlcs :: OHLCOptions -> KrakenT OHLCs
ohlcs = lift . ohlcs_

orderBook :: OrderBookOptions -> KrakenT OrderBook
orderBook = lift . orderBook_

trades :: TradeOptions -> KrakenT Trades
trades = lift . trades_

spreads :: SpreadOptions -> KrakenT Spreads
spreads = lift . spreads_

-----------------------------------------------------------------------------

privateRequest :: ToFormUrlEncoded a =>
                  String ->
                  a ->
                  (Maybe Text -> Maybe Text -> PrivReq a -> ServantT b) ->
                  KrakenT b
privateRequest url d f = do
  Config{..} <- ask
  utcTime <- liftIO getCurrentTime
  let apiKey       = decodeUtf8 configAPIKey
      uri          = BC.pack $ "/" ++ url
      nonce        = fromEnum . utcTimeToPOSIXSeconds $ utcTime
      privReq      = PrivReq nonce configPassword d
      postData     = BL.toStrict $ mimeRender (Proxy :: Proxy FormUrlEncoded)
                                              privReq
      nonceBytes   = BC.pack . show $ nonce
      hashPostData = toBytes (hash (nonceBytes <> postData) :: Digest SHA256)
      
      msg          = uri <> hashPostData
      hmacMsg      = hmac configPrivateKey msg :: HMAC SHA512
      apiSign      = decodeUtf8 . B64.encode . toBytes $ hmacMsg
  lift $ f (Just apiKey) (Just apiSign) privReq

-----------------------------------------------------------------------------

accountBalance :: KrakenT Value
accountBalance = privateRequest 
  (show . safeLink api $ (Proxy :: Proxy AccountBalanceService))
  ()
  accountBalance_

tradeBalance :: TradeBalanceOptions -> KrakenT Value
tradeBalance opts = privateRequest
  (show . safeLink api $ (Proxy :: Proxy TradeBalanceService))
  opts
  tradeBalance_

openOrders :: OpenOrdersOptions -> KrakenT Value
openOrders opts = privateRequest
  (show . safeLink api $ (Proxy :: Proxy OpenOrdersService))
  opts
  openOrders_

tradeVolume :: TradeVolumeOptions -> KrakenT Value
tradeVolume opts = privateRequest
  (show . safeLink api $ (Proxy :: Proxy TradeVolumeService))
  opts
  tradeVolume_

