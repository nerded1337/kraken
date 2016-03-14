# Kraken

Haskell bindings to the [Kraken](https://www.kraken.com/en-us/help/api) API. 

Early stage work in progress.

## To do

* Resolve duplication in private user data functions (first attempt failed due to difficulty in factoring out Proxy's)
* Env vars for api keys: handle read failure, maybe use an env var library (e.g. envy)
* Enforce non-empty lists for relevant requests (e.g. query trades?)
* Prefix enums with enum name
* Untyped requests
* Return types for all services
* Private trading services
* Restrict OHLC bar intervals
* Handle 'since' / last' - JSON can be number or string
* Add tests
* Develop kraken executable into a full command-line tool