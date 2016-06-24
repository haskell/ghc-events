# ghc-events


A Haskell library for parsing .eventlog files emitted by the GHC runtime system.  
The package also includes an executable, `ghc-events` that can be used to display the contents of .eventlog files 

## TODO
* Add example usage/tutorial of the new API to this readme
* Test with GHC 8

## Known Issues
* Writing event logs back to file does not work. It is hard to say how long has this been broken or how difficult will it be to fix

## Changelog:
### 0.5.0.0
* Readme added :)
* Old parser replaced with an incremental implementation 
* General overhaul of the codebase
* Partial Haddock coverage

The 0.5.* releases should be able to handle large event logs and logs that have been cut off abruptly, e.g. from executable runs that resulted in crashes.

This release should be *mostly* backwards compatible, however the "old" way of reading event logs, namely the `readEventLogFromFile`  function is now **deprecated**.   

**NOTE:** Users parsing large logs may notice that this version of the library is noticably slower than the older versions. The incremental parser is at fault here - previous versions of the libray used a "trick" that would let them essentially skip the first step of the mergesort algorithm since `EventBlock`s were already sorted in time order. The new parser reads the file incrementally and cannot take the advantage of this. Apologies for any inconvenience this may have caused.
