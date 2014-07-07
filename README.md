=======
hackage-twitter-bot
===================

A twitter bot for hackage2


TODO: Write description here

## Installation

TODO: Write installation instructions here

## Usage

TODO: Write usage instructions here

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

TODO: Write contribution instructions here



## Configuration

There should be environment variables for everything related to twitter-conduit as well as a config file "config.yaml" with the contents:

```yaml
bitLyKey: "BITLYAPIKEY"
lastErrorTime: "Sun Jul  6 16:06:45 UTC 2014"
```

where lastError time could also be the last time of a restart.