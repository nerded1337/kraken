{-# LANGUAGE TypeApplications #-}

module Kraken.Parse where

import           Control.Applicative ((<|>))
import           Data.Aeson          (FromJSON, Value (Null), parseJSON,
                                      withObject, withText, (.:))
import           Data.Aeson.Types    (Parser)
import           Data.Scientific     (Scientific)
import qualified Data.Text           as T (unpack)

parseMaybeJustNull :: FromJSON a => Maybe Value -> Parser (Maybe a)
parseMaybeJustNull (Just v) = fmap Just (parseJSON v)
parseMaybeJustNull _        = pure Nothing

parseMaybeNull :: FromJSON a => Value -> Parser (Maybe a)
parseMaybeNull Null = pure Nothing
parseMaybeNull v    = parseJSON v

parseResult :: FromJSON a => Value -> Parser a
parseResult = withObject "result" $ \o -> do
  e <- (.:) @[String] o "error"
  case e of
    [] -> o .: "result"
    _  -> (fail . concat . fmap show) e

parseScientific :: Value -> Parser Scientific
parseScientific v =
  withText "scientific" (pure . read . T.unpack) v <|> parseJSON v
