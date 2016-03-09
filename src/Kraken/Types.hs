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
import qualified Data.Text as T (intercalate,pack,toLower)
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
    deriving (Eq,FromJSON,Generic,Ord,Read,Show)

instance Hashable Asset

instance ToText Asset where
  toText = T.pack . show

-----------------------------------------------------------------------------

data AssetInfo = AssetInfo
  { assetinfoDisplayDecimals :: Int
  , assetinfoClass :: Class
  , assetinfoDecimals :: Int
  , assetinfoAltName :: String
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

type AssetPairs = Value

-----------------------------------------------------------------------------

type AssetPairOptions = ()

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
  }

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

type OHLCs = Value

-----------------------------------------------------------------------------

type OHLCOptions = ()

-----------------------------------------------------------------------------

type OrderBook = Value

-----------------------------------------------------------------------------

type OrderBookOptions = ()

-----------------------------------------------------------------------------

type Spreads = Value

-----------------------------------------------------------------------------

type SpreadOptions = ()

-----------------------------------------------------------------------------

type Tickers = Value

-----------------------------------------------------------------------------

type TickerOptions = ()

-----------------------------------------------------------------------------

type Trades = Value

-----------------------------------------------------------------------------

type TradeOptions = ()

-----------------------------------------------------------------------------

newtype Time = Time { unTime :: UTCTime } deriving Show

instance FromJSON Time where
  parseJSON x = do
    r <- parseResult x
    (t :: Int) <- r .: "unixtime"
    return . Time . posixSecondsToUTCTime . fromIntegral $ t

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
