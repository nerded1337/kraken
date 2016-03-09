# Kraken

Haskell bindings to the [Kraken](https://www.kraken.com/en-us/help/api) API. 

Early stage work in progress.

## To do

* Review data type (bytestring,string or text) for keys
* Add all options and return data types
* Add remaining private user data functions
* Add private trading services
* Refactor repetition in service types (first attempt failed - type level DSL does not seem to tolerate type synonyms in some contexts)
* Add tests