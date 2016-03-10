module Kraken.Types where

import           Control.Arrow
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import           Data.Default
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H (fromList,toList)
import           Data.Text (Text)
import qualified Data.Text as T (concat,intercalate,pack,toLower)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Servant.API

-----------------------------------------------------------------------------

type Host = String
type Port = Int

-----------------------------------------------------------------------------

data Asset = 
    XXBT
  | XETH
  | ZCAD
  | ZEUR
  | ZGBP
  | ZJPY
  | ZUSD
    deriving (Eq,FromJSON,Generic,Ord,Read,Show)

instance Hashable Asset

instance ToText Asset where
  toText = T.pack . show

-----------------------------------------------------------------------------

data AssetInfo = AssetInfo
  { assetinfoDisplayDecimals :: Int
  , assetinfoClass :: Class
  , assetinfoDecimals :: Int
  , assetinfoAltName :: Text
  } deriving Show

instance FromJSON AssetInfo where
  parseJSON = withObject "asset info" $ \o -> AssetInfo
    <$> o .: "display_decimals"
    <*> o .: "aclass"
    <*> o .: "decimals"
    <*> o .: "altname"

-----------------------------------------------------------------------------

data AssetOptions = AssetOptions
  { assetClass :: Class
  , assetAssets :: [Asset]
  } deriving Show

instance ToFormUrlEncoded AssetOptions where
  toFormUrlEncoded AssetOptions{..} =
    [ ("aclass",toText assetClass)
    , ("asset",(T.intercalate "," . map toText) assetAssets)
    ]

-----------------------------------------------------------------------------

data AssetPair = AssetPair
  { assetpairBase  :: Asset
  , assetpairQuote :: Asset
  } deriving Show

instance ToText AssetPair where
  toText AssetPair{..} = T.concat [ toText assetpairBase
                                  , toText assetpairQuote
                                  ]

-----------------------------------------------------------------------------

data AssetPairInfo =
    Info
  | Leverage
  | Fees
  | Margin
    deriving (Eq,Ord,Read,Show)

instance ToText AssetPairInfo where
  toText = T.toLower . T.pack . show

-----------------------------------------------------------------------------

type AssetPairs = Value

-----------------------------------------------------------------------------

data AssetPairOptions = AssetPairOptions
  { assetpairInfo  :: AssetPairInfo
  , assetpairPairs :: [AssetPair]
  } deriving Show

instance ToFormUrlEncoded AssetPairOptions where
  toFormUrlEncoded AssetPairOptions{..} =
    [ ("info",toText assetpairInfo)
    , ("pair",(T.intercalate "," . map toText) assetpairPairs)
    ]

-----------------------------------------------------------------------------

newtype Assets = Assets
  { unAssets :: HashMap Asset AssetInfo
  } deriving Show

instance FromJSON Assets where
  parseJSON = parseResult
    >=> parseJSON
    >=> return . Assets . H.fromList . map (first read) . H.toList

-----------------------------------------------------------------------------

data Class =
    Currency
    deriving (Generic,Show)

instance FromJSON Class where
  parseJSON = withText "class" $ \case
    "currency" -> return Currency
    _          -> fail ""

instance ToText Class where
  toText = T.toLower . T.pack . show

-----------------------------------------------------------------------------

data Config = Config
  { configAPIKey     :: ByteString
  , configPrivateKey :: ByteString
  , configPassword   :: Maybe ByteString
  } deriving Show

instance Default Config where
  def = Config
    { configAPIKey     = ""
    , configPrivateKey = ""
    , configPassword   = Nothing
    }

mkConfig :: ByteString -> ByteString -> Maybe ByteString -> Either String Config
mkConfig ak pk pw = case B64.decode pk of
  Right pkd -> Right $ Config ak pkd pw
  Left  e   -> Left e

-----------------------------------------------------------------------------

data OHLCOptions = OHLCOptions
  { ohlcPair :: AssetPair
  , ohlcIntervalMins :: Int
  , ohlcSince :: Maybe Text
  } deriving Show

instance ToFormUrlEncoded OHLCOptions where
  toFormUrlEncoded OHLCOptions{..} =
    [ ("pair",toText ohlcPair)
    , ("interval",T.pack $ show ohlcIntervalMins)
    ]
    ++
    [ ("since",since) | Just since <- [ohlcSince] ]

-----------------------------------------------------------------------------

type OHLCs = Value

-----------------------------------------------------------------------------

type OrderBook = Value

-----------------------------------------------------------------------------

type OrderBookOptions = ()

-----------------------------------------------------------------------------

type Spreads = Value

-----------------------------------------------------------------------------

type SpreadOptions = ()

-----------------------------------------------------------------------------

data TickerOptions = TickerOptions
  { tickerPairs :: [AssetPair]
  } deriving Show

instance ToFormUrlEncoded TickerOptions where
  toFormUrlEncoded TickerOptions{..} =
    [ ("pair",(T.intercalate "," . map toText) tickerPairs)
    ]

-----------------------------------------------------------------------------

type Tickers = Value

-----------------------------------------------------------------------------

newtype Time = Time { unTime :: UTCTime } deriving Show

instance FromJSON Time where
  parseJSON x = do
    r <- parseResult x
    (t :: Int) <- r .: "unixtime"
    return . Time . posixSecondsToUTCTime . fromIntegral $ t

-----------------------------------------------------------------------------

type TradeOptions = ()

-----------------------------------------------------------------------------

type Trades = Value

-----------------------------------------------------------------------------

instance ToFormUrlEncoded () where
  toFormUrlEncoded () = []

data PrivReq a = PrivReq
  { privreqNonce :: Int
  , privreqOTP   :: Maybe ByteString
  , privreqData  :: a
  }

instance (ToFormUrlEncoded a) => ToFormUrlEncoded (PrivReq a) where
  toFormUrlEncoded PrivReq{..} = 
    [ ("nonce",T.pack . show $ privreqNonce) ]
    ++
    [ ("otp"  ,decodeUtf8 otp) | Just otp <- [privreqOTP] ]
    ++
    toFormUrlEncoded privreqData

-----------------------------------------------------------------------------

parseResult :: FromJSON a => Value -> Parser a
parseResult = withObject "result" $ \o -> do
  (e :: [String]) <- o .: "error"
  case e of
    [] -> o .: "result"
    _  -> (fail . concat . map show) e
