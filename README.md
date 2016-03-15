# Kraken

Haskell bindings to the [Kraken](https://www.kraken.com/en-us/help/api) API. 

Early stage work in progress.

## To do

* [ ] Return types for all services
  * [X] Time
  * [X] Assets
  * [X] AssetPairs
  * [X] Ticker
  * [X] OHLC
  * [X] Depth
  * [X] Trades
  * [X] Spread
  * [ ] Balance
  * [ ] TradeBalance
  * [ ] OpenOrders
  * [ ] ClosedOrders
  * [ ] QueryOrders
  * [ ] TradeHistory
  * [ ] QueryTrades
  * [ ] OpenPosition
  * [ ] Ledgers
  * [ ] QueryLedgers
  * [ ] TradeVolume
* [ ] Add 'options' to field names for Options data types
* [ ] Add underscore prefix to field names (for lens derivation)
* [ ] Resolve duplication in private user data functions (first attempt failed due to difficulty in factoring out Proxy's)
* [ ] Extract common functionality in JSON parsers
* [ ] Env vars for api keys: handle read failure, maybe use an env var library (e.g. envy)
* [ ] Enforce non-empty lists for relevant requests (e.g. query trades?)
* [ ] Prefix enums with enum name
* [ ] Untyped requests
* [ ] Private trading services
* [ ] Restrict OHLC bar intervals
* [ ] Handle 'since' / last' - JSON can be number or string
* [ ] Add tests
* [ ] Develop kraken executable into a full command-line tool