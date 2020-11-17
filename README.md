# ghc-events

![build](https://github.com/haskell/ghc-events/workflows/build/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/ghc-events.svg)](https://hackage.haskell.org/package/ghc-events)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/ghc-events.svg)](http://packdeps.haskellers.com/feed?needle=ghc-events)

A Haskell library for parsing .eventlog files emitted by the GHC runtime system.
The package also includes an executable, `ghc-events` that can be used to display the contents of .eventlog files

## TODO
* Add example usage/tutorial of the new API to this readme

## Known Issues
* Writing event logs back to file does not work. It is hard to say how long has this been broken or how difficult will it be to fix ([#14](https://github.com/haskell/ghc-events/issues/14))
