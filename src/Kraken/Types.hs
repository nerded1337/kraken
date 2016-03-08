module Kraken.Types where

import           Control.Arrow
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H (fromList,toList)
import qualified Data.Text as T (intercalate,pack,toLower)
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Servant.API

-----------------------------------------------------------------------------

type Host = String
type Port = Int

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

data Asset = 
    XXBT
  | XETH
    deriving (Eq,FromJSON,Generic,Ord,Read,Show)

instance Hashable Asset

instance ToText Asset where
  toText = T.pack . show

-----------------------------------------------------------------------------

data AssetsOptions = AssetsOptions
  { assetsoptionsClass :: Class
  , assetsoptionsAssets :: [Asset]
  } deriving Show

instance ToFormUrlEncoded AssetsOptions where
  toFormUrlEncoded AssetsOptions{..} =
    [ ("aclass",toText assetsoptionsClass)
    , ("asset",(T.intercalate "," . map toText) assetsoptionsAssets)
    ]

-----------------------------------------------------------------------------

newtype Time = Time { unTime :: UTCTime } deriving Show

instance FromJSON Time where
  parseJSON x = do
    r <- parseResult x
    (t :: Int) <- r .: "unixtime"
    return . Time . posixSecondsToUTCTime . fromIntegral $ t

-----------------------------------------------------------------------------

newtype Assets = Assets
  { unAssets :: HashMap Asset AssetInfo
  } deriving Show

instance FromJSON Assets where
  parseJSON = parseResult
    >=> parseJSON
    >=> return . Assets . H.fromList . map (first read) . H.toList

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

parseResult :: FromJSON a => Value -> Parser a
parseResult = withObject "result" $ \o -> do
  (e :: [String]) <- o .: "error"
  case e of
    [] -> o .: "result"
    _  -> (fail . concat . map show) e
