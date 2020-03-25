{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving       #-}

module Kraken.Types where

import           Control.Arrow          (first)
import           Control.Monad          (guard, mzero, (>=>))
import           Data.Aeson             (FromJSON (parseJSON), Value, withArray, ToJSON, ToJSONKey,
                                         withObject, withScientific, withText,
                                         (.:), (.:?))
import           Data.Aeson.Types       (Parser, parseMaybe)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64 (decode)
import           Data.Char              (toUpper)
import           Data.Default           (Default (def))
import           Data.Hashable          (Hashable)
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as H (delete, filter, fromList, keys,
                                              map, toList)
import           Data.Maybe             (fromJust, isJust)
import           Data.Ratio             ((%))
import           Data.Scientific        (Scientific)
import           Data.Text              (Text)
import qualified Data.Text              as T (concat, intercalate, pack,
                                              toLower, unpack)
import           Data.Text.Encoding     (decodeUtf8)
import           Data.Time              (UTCTime)
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime,
                                         utcTimeToPOSIXSeconds)
import           Data.Vector            ((!))
import           GHC.Exts               (fromList)
import           GHC.Generics           (Generic)
import           Kraken.Parse           (parseMaybeJustNull, parseMaybeNull,
                                         parseResult)
import           Lens.Micro             (over, (&), _head)
import           System.Envy            (FromEnv)
import           Web.FormUrlEncoded     (ToForm (toForm), toListStable)
import           Web.HttpApiData        (ToHttpApiData (toUrlPiece))

newtype Amount =
  Amount
    { amount :: Scientific
    }
  deriving newtype (Show, ToJSON, FromJSON)

maybeAmount :: Amount -> Maybe Amount
maybeAmount a@(Amount s) = if (s == 0) then Nothing else (Just a)

data Asset
  = XXBT
  | XETH
  | ZCAD
  | ZEUR
  | ZGBP
  | ZJPY
  | ZUSD
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (ToJSON, FromJSON)

instance Default Asset where
  def = XXBT

instance Hashable Asset

instance ToHttpApiData Asset where
  toUrlPiece = T.pack . show

data AssetClass =
  Currency
  deriving (Generic, Show, ToJSON)

instance Default AssetClass where
  def = Currency

instance FromJSON AssetClass where
  parseJSON = withText "class" $ \case
    "currency" -> pure Currency
    _          -> fail ""

instance ToHttpApiData AssetClass where
  toUrlPiece = T.toLower . T.pack . show

-----------------------------------------------------------------------------

data AssetInfo =
  AssetInfo
    { assetinfoDisplayDecimals :: Int
    , assetinfoClass :: AssetClass
    , assetinfoDecimals :: Int
    , assetinfoAltName :: Text
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON AssetInfo where
  parseJSON = withObject "asset info" $ \o -> AssetInfo
    <$> o .: "display_decimals"
    <*> o .: "aclass"
    <*> o .: "decimals"
    <*> o .: "altname"

-----------------------------------------------------------------------------

data AssetOptions =
  AssetOptions
    { assetClass :: AssetClass
    , assetAssets :: [Asset]
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Default AssetOptions where
  def = AssetOptions Currency []

instance ToForm AssetOptions where
  toForm AssetOptions {..} =
    fromList $
    [("aclass", toUrlPiece assetClass)] ++
    [ ("asset", (T.intercalate "," . map toUrlPiece) assetAssets)
    | not . null $ assetAssets
    ]

-----------------------------------------------------------------------------

data AssetPair =
  AssetPair
    { assetpairBase :: Asset
    , assetpairQuote :: Asset
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, ToJSON, ToJSONKey)

instance Read AssetPair where
  readsPrec p s | length s >= 8 = do let (bs,qs) = splitAt 4 s
                                     (b,br) <- readsPrec p bs
                                     guard (null br)
                                     (q,qr) <- readsPrec p qs
                                     pure (AssetPair b q,qr)
                | otherwise     = []

instance FromJSON AssetPair where
  parseJSON = withText "AssetPair" (pure . read . T.unpack)

instance Default AssetPair where
  def = AssetPair XXBT ZUSD

instance ToHttpApiData AssetPair where
  toUrlPiece AssetPair {..} =
    T.concat [toUrlPiece assetpairBase, toUrlPiece assetpairQuote]

-----------------------------------------------------------------------------

data AssetPairInfo =
  AssetPairInfo
    { assetpairinfoAltName :: Text
    , assetpairinfoBaseAssetClass :: AssetClass
    , assetpairinfoBaseAsset :: Asset
    , assetpairinfoQuoteAssetClass :: AssetClass
    , assetpairinfoQuoteAsset :: Asset
    , assetpairinfoLot :: String -- TBC
    , assetpairinfoPairDecimals :: Int
    , assetpairinfoLotDecimals :: Int
    , assetpairinfoLotMultiplier :: Int
    , assetpairinfoLeverageBuy :: [Scientific]
    , assetpairinfoLeverageSell :: [Scientific]
    , assetpairinfoFees :: [(Scientific, Scientific)]
    , assetpairinfoFeesMaker :: [(Scientific, Scientific)]
    , assetpairinfoFeeVolumeCurrency :: Asset
    , assetpairinfoMarginCall :: Scientific
    , assetpairinfoMarginStop :: Scientific
    }
  deriving (Show)

instance FromJSON AssetPairInfo where
  parseJSON = withObject "asset pair info" $ \o -> AssetPairInfo
    <$> o .: "altname"
    <*> o .: "aclass_base"
    <*> o .: "base"
    <*> o .: "aclass_quote"
    <*> o .: "quote"
    <*> o .: "lot"
    <*> o .: "pair_decimals"
    <*> o .: "lot_decimals"
    <*> o .: "lot_multiplier"
    <*> o .: "leverage_buy"
    <*> o .: "leverage_sell"
    <*> o .: "fees"
    <*> o .: "fees_maker"
    <*> o .: "fee_volume_currency"
    <*> o .: "margin_call"
    <*> o .: "margin_stop"

-----------------------------------------------------------------------------

data AssetPairs =
  AssetPairs
    { assetpairsPairs :: HashMap AssetPair AssetPairInfo
    }
  deriving (Show)

instance FromJSON AssetPairs where
  parseJSON = parseResult
    >=> parseJSON
    >=> pure . AssetPairs . H.fromList . map (first read) . H.toList

-----------------------------------------------------------------------------

data AssetPairOptions =
  AssetPairOptions
    { assetpairPairs :: [AssetPair]
    }
  deriving (Show)

instance Default AssetPairOptions where
  def = AssetPairOptions []

instance ToForm AssetPairOptions where
  toForm AssetPairOptions{..} = fromList $
    [ ("info","info") ]
    ++
    [ ("pair",(T.intercalate "," . map toUrlPiece) assetpairPairs) | (not . null) assetpairPairs ]

-----------------------------------------------------------------------------

newtype Assets =
  Assets
    { assetsAssets :: HashMap Asset AssetInfo
    }
  deriving (Show)

instance FromJSON Assets where
  parseJSON = parseResult
    >=> parseJSON
    >=> pure . Assets . H.fromList . map (first read) . H.toList

-----------------------------------------------------------------------------

data BalanceInfo =
  BalanceInfo
    { balanceinfoBalance :: Scientific
    }
  deriving (Show)

instance FromJSON BalanceInfo where
  parseJSON = withText "BalanceInfo" $ pure . BalanceInfo . read . T.unpack

-----------------------------------------------------------------------------

data Balance =
  Balance
    { balanceBalances :: HashMap Asset BalanceInfo
    }
  deriving (Show)

instance FromJSON Balance where
  parseJSON = parseResult
    >=> parseJSON
    >=> pure . Balance . H.fromList . map (first read) . H.toList

-----------------------------------------------------------------------------

data ClosedOrders =
  ClosedOrders
    { closedordersOrders :: HashMap TxnId OrderInfo
    , closedordersCount :: Int
    }
  deriving (Show)

instance FromJSON ClosedOrders where
  parseJSON = parseResult >=> withObject "ClosedOrders" (\o -> do
    closedordersOrders <- o .: "closed" >>= parseJSON >>= pure . H.fromList . map (first TxnId) . H.toList
    closedordersCount  <- o .: "count"
    pure ClosedOrders{..})

-----------------------------------------------------------------------------

data ClosedOrdersOptions =
  ClosedOrdersOptions
    { closedordersoptionsIncludeTrades :: Bool
    , closedordersoptionsUserRef :: Maybe Text
    , closedordersoptionsStart :: Maybe TimeBound
    , closedordersoptionsEnd :: Maybe TimeBound
    , closedordersoptionsOffset :: Maybe Int
    , closedordersoptionsCloseTime :: CloseTime
    }
  deriving (Show)

instance Default ClosedOrdersOptions where
  def = ClosedOrdersOptions False Nothing Nothing Nothing Nothing Both

instance ToForm ClosedOrdersOptions where
  toForm ClosedOrdersOptions{..} = fromList $
    [ ("trades",T.toLower . toUrlPiece . show $ closedordersoptionsIncludeTrades ) ]
    ++
    [ ("userref",r) | Just r <- [closedordersoptionsUserRef] ]
    ++
    [("start",toUrlPiece start) | Just start <- [closedordersoptionsStart] ]
    ++
    [("end",toUrlPiece end) | Just end <- [closedordersoptionsEnd] ]
    ++
    [ ("ofs",T.pack . show $ ofs) | Just ofs <- [closedordersoptionsOffset] ]
    ++
    [ ("closetime",T.toLower . T.pack . show $ closedordersoptionsCloseTime )]

-----------------------------------------------------------------------------

data CloseTime
  = Open
  | Close
  | Both
  deriving (Eq, Enum, Ord, Show)

instance Default CloseTime where
  def = Both

-----------------------------------------------------------------------------

data Config =
  Config
    { configApiKey :: ByteString
    , configPrivateKey :: ByteString
    , configPassword :: Maybe ByteString
    }
  deriving (Generic, Show)

mkConfig :: ByteString -> ByteString -> Maybe ByteString -> Either String Config
mkConfig ak pk pw = case B64.decode pk of
  Right pkd -> Right $ Config ak pkd pw
  Left  e   -> Left e

-----------------------------------------------------------------------------

instance FromEnv Config where

-----------------------------------------------------------------------------

type Host = String

-----------------------------------------------------------------------------

data LedgerInfo =
  LedgerInfo
    { ledgerinfoRefId :: RefId
    , ledgerinfoTime :: Timestamp
    , ledgerinfoType :: LedgerType
    , ledgerinfoAclass :: AssetClass
    , ledgerinfoAsset :: Asset
    , ledgerinfoAmount :: Amount
    , ledgerinfoFee :: Amount
    , ledgerinfoBalance :: Amount
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON LedgerInfo where
  parseJSON = withObject "LedgerInfo" $ \o -> LedgerInfo
    <$> o .: "refid"
    <*> o .: "time"
    <*> o .: "type"
    <*> o .: "aclass"
    <*> o .: "asset"
    <*> o .: "amount"
    <*> o .: "fee"
    <*> o .: "balance"

-----------------------------------------------------------------------------

data Ledgers =
  Ledgers
    { ledgersCount :: Int
    , ledgersLedgers :: HashMap RefId LedgerInfo
    }
  deriving (Show)

instance FromJSON Ledgers where
  parseJSON = parseResult >=> withObject "Ledgers" (\o -> do
    ledgersCount   <- o .: "count"
    ledgersLedgers <- o .: "ledger" >>= parseJSON >>= pure . H.fromList . map (first RefId) . H.toList
    pure Ledgers{..})

-----------------------------------------------------------------------------

data LedgersOptions =
  LedgersOptions
    { ledgersAssetClass :: AssetClass
    , ledgersAssets :: [Asset]
    , ledgersType :: Maybe LedgerType
    , ledgersStart :: Maybe TimeBound
    , ledgersEnd :: Maybe TimeBound
    , ledgersOffset :: Maybe Int
    }
  deriving (Show)

instance Default LedgersOptions where
  def = LedgersOptions Currency [] Nothing Nothing Nothing Nothing

instance ToForm LedgersOptions where
  toForm LedgersOptions{..} = fromList $
    [ ("aclass",toUrlPiece ledgersAssetClass) ]
    ++
    [ ("asset",(T.intercalate "," . map toUrlPiece) ledgersAssets) | (not . null) ledgersAssets ]
    ++
    [ ("type",toUrlPiece t) | Just t <- [ledgersType] ]
    ++
    [ ("start",toUrlPiece start) | Just start <- [ledgersStart] ]
    ++
    [ ("end",toUrlPiece end) | Just end <- [ledgersEnd] ]
    ++
    [ ("ofs",T.pack . show $ ofs) | Just ofs <- [ledgersOffset] ]

-----------------------------------------------------------------------------

data LedgerType
  = LedgerType'All
  | LedgerType'Deposit
  | LedgerType'Withdrawal
  | LedgerType'Trade
  | LedgerType'Margin
  deriving (Enum, Eq, Ord, Read, Show, Generic, ToJSON)

instance Default LedgerType where
  def = LedgerType'All

instance FromJSON LedgerType where
  parseJSON = withText "LedgerType" $ pure . read . ("LedgerType'" ++) . over _head toUpper . T.unpack

instance ToHttpApiData LedgerType where
  toUrlPiece = T.toLower . T.pack . drop 11 . show

-----------------------------------------------------------------------------

data OHLC =
  OHLC
    { ohlcTime :: UTCTime
    , ohlcOpen :: Scientific
    , ohlcHigh :: Scientific
    , ohlcLow :: Scientific
    , ohlcClose :: Scientific
    , ohlcVWAP :: Scientific
    , ohlcVol :: Scientific
    , ohlcNumTrades :: Int
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON OHLC where
  parseJSON = withArray "OHLC" $ \v -> OHLC
    <$> fmap (posixSecondsToUTCTime . fromInteger) (parseJSON (v ! 0))
    <*> fmap read (parseJSON (v ! 1))
    <*> fmap read (parseJSON (v ! 2))
    <*> fmap read (parseJSON (v ! 3))
    <*> fmap read (parseJSON (v ! 4))
    <*> fmap read (parseJSON (v ! 5))
    <*> fmap read (parseJSON (v ! 6))
    <*> parseJSON (v ! 7)

-----------------------------------------------------------------------------

data OHLCOptions =
  OHLCOptions
    { ohlcPair :: AssetPair
    , ohlcIntervalMins :: Int
    , ohlcSince :: Maybe UTCTime
    }
  deriving  (Show, Generic)
  deriving anyclass (ToJSON)

instance Default OHLCOptions where
  def = OHLCOptions def 1 Nothing

instance ToForm OHLCOptions where
  toForm OHLCOptions{..} = fromList $
    [ ("pair",toUrlPiece ohlcPair)
    , ("interval",T.pack $ show ohlcIntervalMins)
    ]
    ++
    [ ("since",T.pack $ show $ utcTimeToPOSIXSeconds since) | Just since <- [ohlcSince] ]

-----------------------------------------------------------------------------

data OHLCs =
  OHLCs
    { ohlcsLast :: UTCTime
    , ohlcsOHLCs :: HashMap AssetPair [OHLC]
    }
  deriving  (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON OHLCs where
  parseJSON = parseResult >=> withObject "OHLCs" (\o -> do
    ohlcsLast <- fmap (posixSecondsToUTCTime . fromInteger) (o .: "last")
    let o' = H.map (parseMaybe (parseJSON :: Value -> Parser [OHLC])) (H.delete "last" o)
    let o'' = (H.map fromJust . H.filter isJust) o'
    let ohlcsOHLCs = (H.fromList . map (first (read . T.unpack)) . H.toList) o''
    pure OHLCs{..})

-----------------------------------------------------------------------------

data OpenOrders =
  OpenOrders
    { unOpenOrders :: HashMap TxnId OrderInfo
    }
  deriving  (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON OpenOrders where
  parseJSON = parseResult
    >=> withObject "OpenOrders" (\o -> o .: "open")
    >=> parseJSON
    >=> pure . OpenOrders . H.fromList . map (first TxnId) . H.toList

-----------------------------------------------------------------------------

data OpenOrdersOptions =
  OpenOrdersOptions
    { openordersoptionsIncludeTrades :: Bool
    , openordersoptionsUserRef :: Maybe Text
    }
  deriving  (Show, Generic)
  deriving anyclass (ToJSON)

instance Default OpenOrdersOptions where
  def = OpenOrdersOptions False Nothing

instance ToForm OpenOrdersOptions where
  toForm OpenOrdersOptions{..} = fromList $
    [ ("trades",T.toLower . toUrlPiece . show $ openordersoptionsIncludeTrades ) ]
    ++
    [ ("userref",toUrlPiece r) | Just r <- [openordersoptionsUserRef] ]

-----------------------------------------------------------------------------

data OpenPositionsOptions =
  OpenPositionsOptions
    { openpositionsTxnIds :: [Text]
    , openpositionsIncludePL :: Bool
    }
  deriving  (Show, Generic)
  deriving anyclass (ToJSON)

instance Default OpenPositionsOptions where
  def = OpenPositionsOptions [] False

instance ToForm OpenPositionsOptions where
  toForm OpenPositionsOptions{..} = fromList $
    [ ("txid", T.intercalate "," openpositionsTxnIds )
    , ("docalcs",T.toLower . toUrlPiece . show $ openpositionsIncludePL )
    ]

-----------------------------------------------------------------------------

data OrderBook =
  OrderBook
    { orderbookPair :: AssetPair
    , orderbookBids :: [OrderBookEntry]
    , orderbookAsks :: [OrderBookEntry]
    }
  deriving  (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON OrderBook where
  parseJSON = parseResult >=> parseJSON >=> withObject "OrderBook" (\o -> do
    let p = head $ H.keys o
    ob <- o .: p
    bs <- ob .: "bids"
    as <- ob .: "asks"
    pure $ OrderBook (read $ T.unpack p) bs as)

-----------------------------------------------------------------------------

data OrderBookEntry =
  OrderBookEntry
    { orderbookentryPrice :: Scientific
    , orderbookentryVol :: Scientific
    , orderbookentryTime :: UTCTime
    }
  deriving  (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON OrderBookEntry where
  parseJSON = withArray "OrderBookEntry" $ \v -> OrderBookEntry
    <$> fmap read (parseJSON (v ! 0))
    <*> fmap read (parseJSON (v ! 1))
    <*> fmap (posixSecondsToUTCTime . fromInteger) (parseJSON (v ! 2))

-----------------------------------------------------------------------------

data OrderBookOptions =
  OrderBookOptions
    { orderbookoptionsPair :: AssetPair
    , orderbookoptionsCount :: Maybe Int
    }
  deriving  (Show, Generic)
  deriving anyclass (ToJSON)

instance Default OrderBookOptions where
  def = OrderBookOptions def Nothing

instance ToForm OrderBookOptions where
  toForm OrderBookOptions{..} =  fromList $
    [ ("pair",toUrlPiece orderbookoptionsPair) ]
    ++
    [ ("count",T.pack (show count)) | Just count <- [orderbookoptionsCount]]

-----------------------------------------------------------------------------

data OrderDescription =
  OrderDescription
    { orderdescPair :: Text -- TBC
    , orderdescDir :: OrderDir
    , orderdescType :: OrderType
    , orderdescPrimaryPrice :: Price
    , orderdescSecondaryPrice :: Maybe Price
    , orderdescLeverage :: Text -- TBC
    , orderdescDescription :: Text
    , orderdescClose :: Maybe Text
    }
  deriving  (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON OrderDescription where
  parseJSON = withObject "OrderDescription" $ \o -> OrderDescription
    <$> o .:  "pair"
    <*> o .:  "type"
    <*> o .:  "ordertype"
    <*> o .:  "price"
    <*> fmap maybePrice (o .:  "price2")
    <*> o .:  "leverage"
    <*> o .:  "order"
    <*> o .:? "close"

-----------------------------------------------------------------------------

data OrderDir
  = OrderDir'Buy
  | OrderDir'Sell
  deriving  (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON OrderDir where
  parseJSON = withText "OrderDir" $ \case
    "b"    -> pure OrderDir'Buy
    "buy"  -> pure OrderDir'Buy
    "s"    -> pure OrderDir'Sell
    "sell" -> pure OrderDir'Sell
    _      -> mzero

-----------------------------------------------------------------------------

data OrderInfo =
  OrderInfo
    { orderinfoRefTxnId :: Maybe TxnId
    , orderinfoUserRef :: Maybe UserRef
    , orderinfoStatus :: OrderStatus
    , orderinfoOpenTime :: Timestamp
    , orderinfoStartTime :: Maybe Timestamp
    , orderinfoCloseTime :: Maybe Timestamp
    , orderinfoExpireTime :: Maybe Timestamp
    , orderinfoDescription :: OrderDescription
    , orderinfoStatusReason :: Maybe Text
    , orderinfoVol :: Volume
    , orderinfoVolExecuted :: Volume
    , orderinfoCost :: Amount
    , orderinfoFee :: Amount
    , orderinfoAveragePrice :: Price
    , orderinfoStopPrice :: Maybe Price
    , orderinfoLimitPrice :: Maybe Price
    , orderinfoMisc :: Text -- TBC
    , orderinfoFlags :: Text -- TBC
    , orderinfoTrades :: Maybe [TxnId]
    }
  deriving  (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON OrderInfo where
  parseJSON = withObject "OrderInfo" $ \o -> OrderInfo
    <$> (o .: "refid" >>= parseMaybeNull)
    <*> (o .: "userref" >>= parseMaybeNull)
    <*> o .:  "status"
    <*> o .:  "opentm"
    <*> fmap maybeTimestamp (o .:  "starttm")
    <*> o .:? "closetm"
    <*> fmap maybeTimestamp (o .:  "expiretm")
    <*> o .:  "descr"
    <*> (o .:? "reason" >>= parseMaybeJustNull)
    <*> o .:  "vol"
    <*> o .:  "vol_exec"
    <*> o .:  "cost"
    <*> o .:  "fee"
    <*> o .:  "price"
    <*> o .:? "stopprice"
    <*> o .:? "limitprice"
    <*> o .:  "misc"
    <*> o .:  "oflags"
    <*> o .:?  "trades"

-----------------------------------------------------------------------------

data OrderStatus
  = OrderStatus'Pending
  | OrderStatus'Open
  | OrderStatus'Closed
  | OrderStatus'Canceled
  | OrderStatus'Expired
  deriving stock (Enum, Eq, Ord, Read, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON OrderStatus where
  parseJSON = withText "OrderStatus" $ pure . read . ("OrderStatus'" ++) . over _head toUpper . T.unpack

-----------------------------------------------------------------------------

data OrderType
  = OrderType'Limit
  | OrderType'Market
  | OrderType'StopLoss
  | OrderType'StopLossAndLimit
  | OrderType'StopLossLimit
  | OrderType'StopLossProfit
  | OrderType'StopLossProfitLimit
  | OrderType'TakeProfit
  | OrderType'TakeProfitLimit
  | OrderType'TrailingStop
  | OrderType'TrailingStopLimit
  deriving stock (Enum, Eq, Ord, Read, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON OrderType where
  parseJSON = withText "OrderType" $ \case
    "l"                      -> pure OrderType'Limit
    "limit"                  -> pure OrderType'Limit
    "m"                      -> pure OrderType'Market
    "market"                 -> pure OrderType'Market
    "stop-loss"              -> pure OrderType'StopLoss
    "stop-loss-and-limit"    -> pure OrderType'StopLossAndLimit
    "stop-loss-limit"        -> pure OrderType'StopLossLimit
    "stop-loss-profit"       -> pure OrderType'StopLossProfit
    "stop-loss-profit-limit" -> pure OrderType'StopLossProfitLimit
    "take-profit"            -> pure OrderType'TakeProfit
    "take-profit-limit"      -> pure OrderType'TakeProfitLimit
    "trailing-stop"          -> pure OrderType'TrailingStop
    "trailing-stop-limit"    -> pure OrderType'TrailingStopLimit
    _                        -> mzero

-----------------------------------------------------------------------------

type Port = Int

-----------------------------------------------------------------------------

data PositionInfo =
  PositionInfo
    { positioninfoStatus :: PositionStatus
    , positioninfoClosedPrice :: Price
    , positioninfoClosedCost :: Amount
    , positioninfoClosedFee :: Amount
    , positioninfoClosedVol :: Amount
    , positioninfoClosedMargin :: Amount
    , positioninfoClosedNetPL :: Amount
    , positioninfoClosedTrades :: [Value] -- TBC
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)


instance FromJSON PositionInfo where
  parseJSON = withObject "PositionInfo" $ \o -> PositionInfo
    <$> o .: "posstatus"
    <*> o .: "cprice"
    <*> o .: "ccost"
    <*> o .: "cfee"
    <*> o .: "cvol"
    <*> o .: "cmargin"
    <*> o .: "net"
    <*> o .: "trades"
-----------------------------------------------------------------------------

data PositionStatus
  = PositionStatus'Open
  | PositionStatus'Closed
  deriving (Enum, Eq, Ord, Read, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON PositionStatus where
  parseJSON = withText "PositionStatus" $ pure . read . ("PositionStatus'" ++) . over _head toUpper . T.unpack

-----------------------------------------------------------------------------

newtype Price =
  Price
    { price :: Scientific
    }
  deriving stock (Show, Generic)
  deriving newtype (FromJSON, ToJSON)

maybePrice :: Price -> Maybe Price
maybePrice p@(Price s) = if (s == 0) then Nothing else (Just p)

-----------------------------------------------------------------------------

data PrivateRequest a =
  PrivateRequest
    { privaterequestNonce :: Int
    , privaterequestOTP :: Maybe ByteString
    , privaterequestData :: a
    }

instance (ToForm a) => ToForm (PrivateRequest a) where
  toForm PrivateRequest{..} =  fromList $
    [ ("nonce",T.pack . show $ privaterequestNonce) ]
    ++
    [ ("otp",decodeUtf8 otp) | Just otp <- [privaterequestOTP] ]
    ++
    (toListStable . toForm) privaterequestData

-----------------------------------------------------------------------------

newtype RefId =
  RefId
    { refId :: Text
    }
  deriving newtype (Eq, Hashable, Show, FromJSON, ToJSON, ToJSONKey)

-----------------------------------------------------------------------------

data QueryLedgers =
  QueryLedgers
    { queryledgersLedgers :: HashMap RefId LedgerInfo
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON QueryLedgers where
  parseJSON = parseResult >=> parseJSON >=> pure . QueryLedgers . H.fromList . map (first RefId) . H.toList

-----------------------------------------------------------------------------

data QueryLedgersOptions =
  QueryLedgersOptions
    { queryledgersIds :: [Text]
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Default QueryLedgersOptions where
  def = QueryLedgersOptions []

instance ToForm QueryLedgersOptions where
  toForm QueryLedgersOptions{..} = fromList $
    [ ("id", T.intercalate "," queryledgersIds) ]

-----------------------------------------------------------------------------

newtype QueryOrders =
  QueryOrders
    { unQueryOrders :: HashMap TxnId OrderInfo
    }
  deriving stock (Show, Generic)
  deriving newtype (ToJSON)

instance FromJSON QueryOrders where
  parseJSON = parseResult >=> parseJSON
    >=> pure . QueryOrders . H.fromList . map (first TxnId) . H.toList

-----------------------------------------------------------------------------

data QueryOrdersOptions = QueryOrdersOptions
  { queryordersIncludeTrades :: Bool
  , queryordersUserRef       :: Maybe Text
  , queryordersTxnIds        :: [Text]
  } deriving Show

instance Default QueryOrdersOptions where
  def = QueryOrdersOptions False Nothing [""]

instance ToForm QueryOrdersOptions where
  toForm QueryOrdersOptions{..} = fromList $
    [ ("trades",T.toLower . toUrlPiece . show $ queryordersIncludeTrades ) ]
    ++
    [ ("userref",toUrlPiece r) | Just r <- [queryordersUserRef] ]
    ++
    [ ("txid", T.intercalate "," queryordersTxnIds ) ]

-----------------------------------------------------------------------------

newtype QueryTrades =
  QueryTrades
    { querytradesTrades :: HashMap TxnId TradeHistoryInfo
    }
  deriving newtype (Show, ToJSON)

instance FromJSON QueryTrades where
  parseJSON = parseResult >=> parseJSON >=> pure . QueryTrades . H.fromList . map (first TxnId) . H.toList

-----------------------------------------------------------------------------

data QueryTradesOptions =
  QueryTradesOptions
    { querytradesTxnIds :: [Text]
    , querytradesIncludeTrades :: Bool
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Default QueryTradesOptions where
  def = QueryTradesOptions [] False

instance ToForm QueryTradesOptions where
  toForm QueryTradesOptions{..} = fromList $
    [ ("txid", T.intercalate "," querytradesTxnIds ) ]
    ++
    [ ("trades",T.toLower . toUrlPiece . show $ querytradesIncludeTrades ) ]

-----------------------------------------------------------------------------

data SpreadInfo =
  SpreadInfo
    { spreadinfoTime :: UTCTime
    , spreadinfoBid :: Scientific
    , spreadinfoAsk :: Scientific
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON SpreadInfo where
  parseJSON = withArray "SpreadInfo" $ \v -> SpreadInfo
    <$> fmap (posixSecondsToUTCTime . fromInteger) (parseJSON (v ! 0))
    <*> fmap read (parseJSON (v ! 1))
    <*> fmap read (parseJSON (v ! 2))

-----------------------------------------------------------------------------

data SpreadOptions =
  SpreadOptions
    { spreadPair :: AssetPair
    , spreadSince :: Maybe Text
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Default SpreadOptions where
  def = SpreadOptions def Nothing

instance ToForm SpreadOptions where
  toForm SpreadOptions{..} = fromList $
    [ ("pair",toUrlPiece spreadPair)]
    ++
    [ ("since",since) | Just since <- [spreadSince] ]

-----------------------------------------------------------------------------

data Spreads =
  Spreads
    { spreadsPair :: AssetPair
    , spreadsLast :: UTCTime
    , spreadsSpreads :: [SpreadInfo]
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON Spreads where
  parseJSON = parseResult >=> withObject "Spreads" (\o -> do
    lt <- (o .: "last") >>= parseJSON
    let l = (posixSecondsToUTCTime . fromInteger) lt
    let (p,ssj) = (head . H.toList . H.delete "last") o
    ss <- parseJSON ssj
    pure $ Spreads (read $ T.unpack p) l ss)

-----------------------------------------------------------------------------

data TickerInfo =
  TickerInfo
    { tickerAskPrice :: Scientific
    , tickerAskVol :: Scientific
    , tickerBidPrice :: Scientific
    , tickerBidVol :: Scientific
    , tickerLastTradePrice :: Scientific
    , tickerLastTradeVol :: Scientific
    , tickerVolToday :: Scientific
    , tickerVol24Hours :: Scientific
    , tickerVWAPToday :: Scientific
    , tickerVWAP24Hours :: Scientific
    , tickerNumTradesToday :: Int
    , tickerNumTrades24Hours :: Int
    , tickerLowToday :: Scientific
    , tickerLow24Hours :: Scientific
    , tickerHighToday :: Scientific
    , tickerHigh24Hours :: Scientific
    , tickerOpen :: Scientific
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON TickerInfo where
  parseJSON = withObject "ticker info" $ \o -> do
    [tickerAskPrice,_,tickerAskVol] <- (map read) <$> o .: "a"
    [tickerBidPrice,_,tickerBidVol] <- (map read) <$> o .: "b"
    [tickerLastTradePrice,tickerLastTradeVol] <- (map read) <$> o .: "c"
    [tickerVolToday,tickerVol24Hours] <- (map read) <$> o .: "v"
    [tickerVWAPToday,tickerVWAP24Hours] <- (map read) <$> o .: "p"
    [tickerNumTradesToday,tickerNumTrades24Hours] <- o .: "t" :: Parser [Int]
    [tickerLowToday,tickerLow24Hours] <- (map read) <$> o .: "l"
    [tickerHighToday,tickerHigh24Hours] <- (map read) <$> o .: "h"
    tickerOpen <- read <$> o .: "o"
    pure TickerInfo{..}

-----------------------------------------------------------------------------

newtype TickerOptions =
  TickerOptions
    { tickerPairs :: [AssetPair]
    }
  deriving newtype (Show, ToJSON)

instance Default TickerOptions where
  def = TickerOptions []

instance ToForm TickerOptions where
  toForm TickerOptions{..} = fromList $
    [ ("pair",(T.intercalate "," . map toUrlPiece) tickerPairs)
    ]

-----------------------------------------------------------------------------

newtype Ticker =
  Ticker
    { unTicker :: HashMap AssetPair TickerInfo
    }
  deriving newtype (Show, ToJSON)

instance FromJSON Ticker where
  parseJSON = parseResult
    >=> parseJSON
    >=> pure . Ticker . H.fromList . map (first read) . H.toList

-----------------------------------------------------------------------------

newtype Time =
  Time
    { unTime :: UTCTime
    }
  deriving newtype (Show, ToJSON)

instance FromJSON Time where
  parseJSON x = do
    r <- parseResult x
    (t :: Int) <- r .: "unixtime"
    pure . Time . posixSecondsToUTCTime . fromIntegral $ t

-----------------------------------------------------------------------------

data TimeBound
  = TimeBound'DateTime UTCTime
  | TimeBound'TxnId Text
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance ToHttpApiData TimeBound where
  toUrlPiece (TimeBound'DateTime ut) = T.pack . show . utcTimeToPOSIXSeconds $ ut
  toUrlPiece (TimeBound'TxnId ti) = ti

-----------------------------------------------------------------------------

newtype Timestamp =
  Timestamp
    { timestamp :: UTCTime
    }
  deriving newtype (Show, ToJSON)

instance FromJSON Timestamp where
  parseJSON = withScientific "Timestamp" $ pure . Timestamp . posixSecondsToUTCTime . fromRational . toRational

maybeTimestamp :: Timestamp -> Maybe Timestamp
maybeTimestamp ts@(Timestamp t) = if (t == posixSecondsToUTCTime 0) then Nothing else (Just ts)

-----------------------------------------------------------------------------

data TradeBalance =
  TradeBalance
    { tradebalanceEquivBalance :: Scientific
    , tradebalanceTradeBalance :: Scientific
    , tradebalanceMarginOpen :: Scientific
    , tradebalanceUnrealizedNetPLOpen :: Scientific
    , tradebalanceCostBasisOpen :: Scientific
    , tradebalanceFloatingValOpen :: Scientific
    , tradebalanceEquity :: Scientific
    , tradebalanceFreeMargin :: Scientific
    , tradebalanceMarginLevel :: Maybe Scientific
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON TradeBalance where
  parseJSON = parseResult >=> withObject "TradeBalance" (\o -> do
    tradebalanceEquivBalance <- read <$> o .: "eb"
    tradebalanceTradeBalance <- read <$> o .: "tb"
    tradebalanceMarginOpen <- read <$> o .: "m"
    tradebalanceUnrealizedNetPLOpen <- read <$> o .: "n"
    tradebalanceCostBasisOpen <- read <$> o .: "c"
    tradebalanceFloatingValOpen <- read <$> o .: "v"
    tradebalanceEquity <- read <$> o .: "e"
    tradebalanceFreeMargin <- read <$> o .: "mf"
    tradebalanceMarginLevel <- (fmap read) <$> o .:? "ml"
    pure TradeBalance{..})

-----------------------------------------------------------------------------

data TradeBalanceOptions =
  TradeBalanceOptions
    { tradebalanceAssetClass :: Maybe AssetClass
    , tradebalanceAsset :: Asset
    }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Default TradeBalanceOptions where
  def = TradeBalanceOptions (Just Currency) ZUSD

instance ToForm TradeBalanceOptions where
  toForm TradeBalanceOptions{..} = fromList $
    [ ("aclass",toUrlPiece c) | Just c <- [tradebalanceAssetClass] ]
    ++
    [ ("asset",toUrlPiece tradebalanceAsset) ]

-----------------------------------------------------------------------------

data TradeHistoryInfo =
  TradeHistoryInfo
    { tradehistoryinfoTxnId :: TxnId
    , tradehistoryinfoPair :: AssetPair
    , tradehistoryinfoTime :: Timestamp
    , tradehistoryinfoDir :: OrderDir
    , tradehistoryinfoType :: OrderType
    , tradehistoryinfoPrice :: Price
    , tradehistoryinfoCost :: Amount
    , tradehistoryinfoFee :: Amount
    , tradehistoryinfoVol :: Volume
    , tradehistoryinfoInitialMargin :: Amount
    , tradehistoryinfoMisc :: Text -- TBC
    , tradehistoryinfoClosing :: Maybe Value -- TBC
    , tradehistoryinfoPosition :: Maybe PositionInfo
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON TradeHistoryInfo where
  parseJSON v = v & withObject "TradeHistoryInfo" (\o -> TradeHistoryInfo
    <$> o .:  "ordertxid"
    <*> o .:  "pair"
    <*> o .:  "time"
    <*> o .:  "type"
    <*> o .:  "ordertype"
    <*> o .:  "price"
    <*> o .:  "cost"
    <*> o .:  "fee"
    <*> o .:  "vol"
    <*> o .:  "margin"
    <*> o .:  "misc"
    <*> o .:? "closing"
    <*> (o .:? "posstatus" >>= \case
          Nothing                    -> pure Nothing
          Just (_ :: PositionStatus) -> fmap Just (parseJSON v)))

-----------------------------------------------------------------------------

data TradeInfo =
  TradeInfo
    { tradeinfoPrice :: Scientific
    , tradeinfoVol :: Scientific
    , tradeinfoTime :: UTCTime
    , tradeinfoDir :: OrderDir
    , tradeinfoType :: OrderType
    , tradeinfoMisc :: Text
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON TradeInfo where
  parseJSON = withArray "TradeInfo" $ \v -> TradeInfo
    <$> fmap read (parseJSON (v ! 0))
    <*> fmap read (parseJSON (v ! 1))
    <*> fmap (posixSecondsToUTCTime . fromInteger) (parseJSON (v ! 2))
    <*> parseJSON (v ! 3)
    <*> parseJSON (v ! 4)
    <*> parseJSON (v ! 5)

-----------------------------------------------------------------------------

data Trades =
  Trades
    { tradesPair :: AssetPair
    , tradesLast :: UTCTime
    , tradesTrades :: [TradeInfo]
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON Trades where
  parseJSON = parseResult >=> withObject "Trades" (\o -> do
    lt <- (o .: "last") >>= parseJSON
    let l = (posixSecondsToUTCTime . fromRational . (% 1000000000) . fromInteger . read) lt
    let (p,tsj) = (head . H.toList . H.delete "last") o
    ts <- parseJSON tsj
    pure $ Trades (read $ T.unpack p) l ts)

-----------------------------------------------------------------------------

data TradesHistory =
  TradesHistory
    { tradeshistoryTrades :: HashMap TxnId TradeHistoryInfo
    , tradeshistoryCount :: Int
    }
  deriving  (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON TradesHistory where
  parseJSON = parseResult >=> withObject "TradesHistory" (\o -> do
    tradeshistoryTrades <- o .: "trades" >>= parseJSON >>= pure . H.fromList . map (first TxnId) . H.toList
    tradeshistoryCount   <- o .: "count"
    pure TradesHistory{..})

-----------------------------------------------------------------------------

data TradesHistoryOptions =
  TradesHistoryOptions
    { tradeshistoryoptionsType :: Maybe TradeType
    , tradeshistoryoptionsIncludeTrades :: Bool
    , tradeshistoryoptionsStart :: Maybe TimeBound
    , tradeshistoryoptionsEnd :: Maybe TimeBound
    , tradeshistoryoptionsOffset :: Maybe Int
    }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Default TradesHistoryOptions where
  def = TradesHistoryOptions Nothing False Nothing Nothing Nothing

instance ToForm TradesHistoryOptions where
  toForm TradesHistoryOptions{..} = fromList $
    [ ("type",toUrlPiece t) | Just t <- [tradeshistoryoptionsType] ]
    ++
    [ ("trades",T.toLower . toUrlPiece . show $ tradeshistoryoptionsIncludeTrades ) ]
    ++
    [("start",toUrlPiece start) | Just start <- [tradeshistoryoptionsStart] ]
    ++
    [("end",toUrlPiece end) | Just end <- [tradeshistoryoptionsEnd] ]
    ++
    [ ("ofs",T.pack . show $ ofs) | Just ofs <- [tradeshistoryoptionsOffset] ]

-----------------------------------------------------------------------------

data TradesOptions =
  TradesOptions
    { tradesoptionsPair :: AssetPair
    , tradesoptionsSince :: Maybe Text
    }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Default TradesOptions where
  def = TradesOptions def Nothing

instance ToForm TradesOptions where
  toForm TradesOptions{..} = fromList $
    [ ("pair",toUrlPiece tradesoptionsPair)]
    ++
    [ ("since",since) | Just since <- [tradesoptionsSince] ]

-----------------------------------------------------------------------------

data TradeType
  = AllTradeTypes
  | AnyPosition
  | ClosedPosition
  | ClosingPosition
  | NoPosition
  deriving stock (Enum, Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Default TradeType where
  def = AllTradeTypes

instance ToHttpApiData TradeType where
  toUrlPiece = \case
    AllTradeTypes   -> "all"
    AnyPosition     -> "any position"
    ClosedPosition  -> "closed position"
    ClosingPosition -> "closing position"
    NoPosition      -> "no position"

-----------------------------------------------------------------------------

data TradeVolumeOptions =
  TradeVolumeOptions
    { tradevolumeFeePairs :: [AssetPair]
    }
  deriving (Show)

instance Default TradeVolumeOptions where
  def = TradeVolumeOptions []

instance ToForm TradeVolumeOptions where
  toForm TradeVolumeOptions{..} = fromList $
    case tradevolumeFeePairs of
      [] -> []
      _  -> [ ("pair",(T.intercalate "," . map toUrlPiece) tradevolumeFeePairs)
            , ("fee-info","true")
            ]

newtype TxnId =
  TxnId
    { txnId :: Text
    }
  deriving newtype (Eq, Hashable, Show, FromJSON, ToJSON, ToJSONKey)

newtype UserRef =
  UserRef
    { userRef :: Text
    }
  deriving newtype (FromJSON, ToJSON, Hashable, Show)

newtype Volume =
  Volume
    { volume :: Scientific
    }
  deriving newtype (FromJSON, ToJSON, Hashable, Show)

instance ToForm () where
  toForm () = fromList []

