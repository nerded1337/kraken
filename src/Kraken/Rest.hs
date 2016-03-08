module Kraken.Rest where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Crypto.Hash
import           Data.Aeson.Types
import           Data.Byteable
import           Data.ByteString
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.Default
import           Data.Monoid
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T (pack)
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

type ServantT = EitherT ServantError IO
type KrakenT  = ReaderT Config ServantT

runKraken :: Config -> KrakenT a -> IO (Either ServantError a)
runKraken cfg = runEitherT . flip runReaderT cfg

-----------------------------------------------------------------------------

type KrakenAPI          = TimeService
                          :<|>
                          AssetsService
                          :<|>
                          AccBalService
type TimeService        = APIVersion
                          :> Public
                          :> "Time"
                          :> Get '[JSON] Time
type AssetsService      = APIVersion
                          :> Public
                          :> "Assets"
                          :> ReqBody '[FormUrlEncoded] AssetsOptions
                          :> Post '[JSON] Assets
type AccBalService      = APIVersion
                          :> Private
                          :> "Balance"
                          :> Header "API-Key" Text
                          :> Header "API-Sign" Text
                          :> ReqBody '[FormUrlEncoded] (PrivReq ())
                          :> Post '[JSON] Value
type APIVersion         = "0"
type Public             = "public"
type Private            = "private"

{-
type KrakenAPI          = APIVersion :> Services
type PrivateRequest a b = Header "API-Key" Text
                          :> Header "API-Sign" Text
                          :> PublicRequest a b
type PublicRequest a b  = ReqBody '[FormUrlEncoded] a
                          :> Post '[JSON] b
-}

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

api :: Proxy KrakenAPI
api = Proxy

-----------------------------------------------------------------------------

time_      :: ServantT Time
assets_    :: AssetsOptions -> ServantT Assets
balance_   :: Maybe Text -> Maybe Text -> PrivReq () -> ServantT Value

time_
  :<|> assets_
  :<|> balance_ = client api (BaseUrl Https restHost restPort)

-----------------------------------------------------------------------------

time :: KrakenT Time
time = lift time_

assets :: AssetsOptions -> KrakenT Assets
assets = lift . assets_

balance :: KrakenT Value
balance = do
  Config{..} <- ask
  utcTime <- liftIO getCurrentTime
  let apiKey       = decodeUtf8 configAPIKey
      apiSign      = decodeUtf8 . B64.encode . toBytes $ hmacMsg
      msg          = uri <> hashPostData
      hmacMsg      = hmac configPrivateKey msg :: HMAC SHA512
      privReq      = PrivReq nonce configPassword ()
      postData     = BL.toStrict $ mimeRender (Proxy :: Proxy FormUrlEncoded)
                                              privReq
      hashPostData = toBytes (hash (nonceBytes <> postData) :: Digest SHA256)
      nonce        = fromEnum . utcTimeToPOSIXSeconds $ utcTime
      nonceBytes   = BC.pack . show $ nonce
      uri          = BC.pack $ "/" ++ (show $ safeLink api
                                               (Proxy :: Proxy AccBalService))
  lift $ balance_ (Just apiKey) (Just apiSign) privReq

