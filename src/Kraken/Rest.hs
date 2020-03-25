{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Kraken.Rest where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Crypto.Hash                (Digest, HMAC, SHA256, SHA512, hash,
                                             hmac)
import           Data.Aeson.Types           (Value)
import           Data.Byteable              (toBytes)
import qualified Data.ByteString.Base64     as B64 (encode)
import qualified Data.ByteString.Char8      as BC (pack)
import qualified Data.ByteString.Lazy       as BL (toStrict)
import           Data.Monoid                ()
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8)
import           Data.Time                  (getCurrentTime)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)
import           GHC.TypeLits               (Symbol)
import           Kraken.Types               (AssetOptions, AssetPairOptions,
                                             AssetPairs, Assets, Balance,
                                             ClosedOrders, ClosedOrdersOptions,
                                             Config (Config), Host, Ledgers,
                                             LedgersOptions, OHLCOptions, OHLCs,
                                             OpenOrders, OpenOrdersOptions,
                                             OpenPositionsOptions, OrderBook,
                                             OrderBookOptions, Port,
                                             PrivateRequest (PrivateRequest),
                                             QueryLedgers, QueryLedgersOptions,
                                             QueryOrders, QueryOrdersOptions,
                                             QueryTrades, QueryTradesOptions,
                                             SpreadOptions, Spreads, Ticker,
                                             TickerOptions, Time, TradeBalance,
                                             TradeBalanceOptions,
                                             TradeVolumeOptions, Trades,
                                             TradesHistory,
                                             TradesHistoryOptions,
                                             TradesOptions, configApiKey,
                                             configPassword, configPrivateKey)
import           Network.HTTP.Client.TLS    (newTlsManager)
import           Servant.API                ((:<|>) (..), (:>), FormUrlEncoded,
                                             Header, JSON, Post, ReqBody,
                                             mimeRender, safeLink)
import           Servant.Client             (BaseUrl (BaseUrl), ClientError,
                                             ClientM, Scheme (Https), client,
                                             mkClientEnv, runClientM)
import           Web.FormUrlEncoded         (ToForm)

restHost :: Host
restHost = "api.kraken.com"

restPort :: Port
restPort = 443

type KrakenT  = ReaderT Config ClientM

runKraken :: Config -> KrakenT a -> IO (Either ClientError a)
runKraken cfg ka = do
  mgr <- newTlsManager
  let env = mkClientEnv mgr baseUrl
  flip runClientM env $ runReaderT ka cfg
  where
    baseUrl = BaseUrl Https restHost restPort ""

type KrakenAPI             = TimeService
                        :<|> AssetService
                        :<|> AssetPairService
                        :<|> TickerService
                        :<|> OHLCService
                        :<|> OrderBookService
                        :<|> TradesService
                        :<|> SpreadService
                        :<|> BalanceService
                        :<|> TradeBalanceService
                        :<|> OpenOrdersService
                        :<|> ClosedOrdersService
                        :<|> QueryOrdersService
                        :<|> TradesHistoryService
                        :<|> QueryTradesService
                        :<|> OpenPositionsService
                        :<|> LedgersService
                        :<|> QueryLedgersService
                        :<|> TradeVolumeService

type TimeService           = PublicService  "Time"          ()                   Time
type AssetService          = PublicService  "Assets"        AssetOptions         Assets
type AssetPairService      = PublicService  "AssetPairs"    AssetPairOptions     AssetPairs
type TickerService         = PublicService  "Ticker"        TickerOptions        Ticker
type OHLCService           = PublicService  "OHLC"          OHLCOptions          OHLCs
type OrderBookService      = PublicService  "Depth"         OrderBookOptions     OrderBook
type TradesService         = PublicService  "Trades"        TradesOptions        Trades
type SpreadService         = PublicService  "Spread"        SpreadOptions        Spreads
type BalanceService        = PrivateService "Balance"       ()                   Balance
type TradeBalanceService   = PrivateService "TradeBalance"  TradeBalanceOptions  TradeBalance
type OpenOrdersService     = PrivateService "OpenOrders"    OpenOrdersOptions    OpenOrders
type ClosedOrdersService   = PrivateService "ClosedOrders"  ClosedOrdersOptions  ClosedOrders
type QueryOrdersService    = PrivateService "QueryOrders"   QueryOrdersOptions   QueryOrders
type TradesHistoryService  = PrivateService "TradesHistory" TradesHistoryOptions TradesHistory
type QueryTradesService    = PrivateService "QueryTrades"   QueryTradesOptions   QueryTrades
type OpenPositionsService  = PrivateService "OpenPositions" OpenPositionsOptions Value
type LedgersService        = PrivateService "Ledgers"       LedgersOptions       Ledgers
type QueryLedgersService   = PrivateService "QueryLedgers"  QueryLedgersOptions  QueryLedgers
type TradeVolumeService    = PrivateService "TradeVolume"   TradeVolumeOptions   Value

type APIVersion            = "0"
type Public                = "public"
type Private               = "private"

type PublicService
     (a :: Symbol) b c     = APIVersion
                             :> Public
                             :> a
                             :> ReqBody '[FormUrlEncoded] b
                             :> Post '[JSON] c
type PrivateService
     (a :: Symbol) b c     = APIVersion
                             :> Private
                             :> a
                             :> Header "API-Key" Text
                             :> Header "API-Sign" Text
                             :> ReqBody '[FormUrlEncoded] (PrivateRequest b)
                             :> Post '[JSON] c

api :: Proxy KrakenAPI
api = Proxy

time_          :: () -> ClientM Time
assets_        :: AssetOptions -> ClientM Assets
assetPairs_    :: AssetPairOptions -> ClientM AssetPairs
ticker_        :: TickerOptions -> ClientM Ticker
ohlcs_         :: OHLCOptions -> ClientM OHLCs
orderBook_     :: OrderBookOptions -> ClientM OrderBook
trades_        :: TradesOptions -> ClientM Trades
spreads_       :: SpreadOptions -> ClientM Spreads
balance_       :: Maybe Text -> Maybe Text -> PrivateRequest () -> ClientM Balance
tradeBalance_  :: Maybe Text -> Maybe Text -> PrivateRequest TradeBalanceOptions -> ClientM TradeBalance
openOrders_    :: Maybe Text -> Maybe Text -> PrivateRequest OpenOrdersOptions -> ClientM OpenOrders
closedOrders_  :: Maybe Text -> Maybe Text -> PrivateRequest ClosedOrdersOptions -> ClientM ClosedOrders
queryOrders_   :: Maybe Text -> Maybe Text -> PrivateRequest QueryOrdersOptions -> ClientM QueryOrders
tradesHistory_ :: Maybe Text -> Maybe Text -> PrivateRequest TradesHistoryOptions -> ClientM TradesHistory
queryTrades_   :: Maybe Text -> Maybe Text -> PrivateRequest QueryTradesOptions -> ClientM QueryTrades
openPositions_ :: Maybe Text -> Maybe Text -> PrivateRequest OpenPositionsOptions -> ClientM Value
ledgers_       :: Maybe Text -> Maybe Text -> PrivateRequest LedgersOptions -> ClientM Ledgers
queryLedgers_  :: Maybe Text -> Maybe Text -> PrivateRequest QueryLedgersOptions -> ClientM QueryLedgers
tradeVolume_   :: Maybe Text -> Maybe Text -> PrivateRequest TradeVolumeOptions -> ClientM Value

time_
  :<|> assets_
  :<|> assetPairs_
  :<|> ticker_
  :<|> ohlcs_
  :<|> orderBook_
  :<|> trades_
  :<|> spreads_
  :<|> balance_
  :<|> tradeBalance_
  :<|> openOrders_
  :<|> closedOrders_
  :<|> queryOrders_
  :<|> tradesHistory_
  :<|> queryTrades_
  :<|> openPositions_
  :<|> ledgers_
  :<|> queryLedgers_
  :<|> tradeVolume_  = client api

privateRequest :: ToForm a =>
                  String ->
                  a ->
                  (Maybe Text -> Maybe Text -> PrivateRequest a -> ClientM b) ->
                  KrakenT b
privateRequest url d f = do
  Config {..} <- ask
  utcTime <- liftIO getCurrentTime
  let apiKey       = decodeUtf8 configApiKey
      uri          = BC.pack $ "/" <> url
      nonce        = fromEnum . utcTimeToPOSIXSeconds $ utcTime
      privReq      = PrivateRequest nonce configPassword d
      postData     =
        BL.toStrict $ mimeRender (Proxy @FormUrlEncoded) privReq
      nonceBytes   = BC.pack . show $ nonce
      hashPostData = toBytes (hash (nonceBytes <> postData) :: Digest SHA256)
      msg          = uri <> hashPostData
      hmacMsg      = hmac configPrivateKey msg :: HMAC SHA512
      apiSign      = decodeUtf8 . B64.encode . toBytes $ hmacMsg
  lift $ f (Just apiKey) (Just apiSign) privReq

time :: KrakenT Time
time = lift $ time_ ()

assets :: AssetOptions -> KrakenT Assets
assets = lift . assets_

assetPairs :: AssetPairOptions -> KrakenT AssetPairs
assetPairs = lift . assetPairs_

ticker :: TickerOptions -> KrakenT Ticker
ticker = lift . ticker_

ohlcs :: OHLCOptions -> KrakenT OHLCs
ohlcs = lift . ohlcs_

orderBook :: OrderBookOptions -> KrakenT OrderBook
orderBook = lift . orderBook_

trades :: TradesOptions -> KrakenT Trades
trades = lift . trades_

spreads :: SpreadOptions -> KrakenT Spreads
spreads = lift . spreads_

balance :: KrakenT Balance
balance =
  privateRequest
    (show . safeLink api $ (Proxy @BalanceService))
    ()
    balance_

tradeBalance :: TradeBalanceOptions -> KrakenT TradeBalance
tradeBalance opts =
  privateRequest
    (show . safeLink api $ (Proxy @TradeBalanceService))
    opts
    tradeBalance_

openOrders :: OpenOrdersOptions -> KrakenT OpenOrders
openOrders opts =
  privateRequest
    (show . safeLink api $ (Proxy @OpenOrdersService))
    opts
    openOrders_

closedOrders :: ClosedOrdersOptions -> KrakenT ClosedOrders
closedOrders opts =
  privateRequest
    (show . safeLink api $ (Proxy @ClosedOrdersService))
    opts
    closedOrders_

queryOrders :: QueryOrdersOptions -> KrakenT QueryOrders
queryOrders opts =
  privateRequest
    (show . safeLink api $ (Proxy @QueryOrdersService))
    opts
    queryOrders_

tradesHistory :: TradesHistoryOptions -> KrakenT TradesHistory
tradesHistory opts =
  privateRequest
    (show . safeLink api $ (Proxy @TradesHistoryService))
    opts
    tradesHistory_

queryTrades :: QueryTradesOptions -> KrakenT QueryTrades
queryTrades opts =
  privateRequest
    (show . safeLink api $ (Proxy @QueryTradesService))
    opts
    queryTrades_

openPositions :: OpenPositionsOptions -> KrakenT Value
openPositions opts =
  privateRequest
    (show . safeLink api $ (Proxy @OpenPositionsService))
    opts
    openPositions_

ledgers :: LedgersOptions -> KrakenT Ledgers
ledgers opts =
  privateRequest
    (show . safeLink api $ (Proxy @LedgersService))
    opts
    ledgers_

queryLedgers :: QueryLedgersOptions -> KrakenT QueryLedgers
queryLedgers opts =
  privateRequest
    (show . safeLink api $ (Proxy @QueryLedgersService))
    opts
    queryLedgers_

tradeVolume :: TradeVolumeOptions -> KrakenT Value
tradeVolume opts =
  privateRequest
    (show . safeLink api $ (Proxy @TradeVolumeService))
    opts
    tradeVolume_
