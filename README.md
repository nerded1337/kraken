# Kraken

Haskell bindings to the [Kraken](https://www.kraken.com/en-us/help/api) API. 

Early stage work in progress.

## To do

* Add all options and return data types
* Add remaining private user data functions
* Add private trading services
* Defaults for request options
* Refactor repetition in service types (first attempt failed - type level DSL does not seem to tolerate type synonyms in some contexts)
* Restrict OHLC bar intervals
* Handle 'since' / last' - JSON can be number or string
* Add tests
* Develop kraken executable into a full command-line tool