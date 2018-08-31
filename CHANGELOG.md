# Change Log

## 0.9.0 - unreleased

* Event parser (`getEvent`) now reads extra unparsed data after events (instead of skipping them as before) and put it in the new `evExtras` field. This field is used to implement round-trip property of `getEvent`/`putEvent`. See [#42](https://github.com/haskell/ghc-events/issues/42) for more details.
    * This is a breaking change (a new field `evExtras` added to `Event`)

## 0.8.0 - 2018-07-11

* Add HeapProfBreakdownClosureType ([#33](https://github.com/haskell/ghc-events/pull/33), [#39](https://github.com/haskell/ghc-events/pull/39))
    * This is a breaking change
* Test with newer GHCs ([#40](https://github.com/haskell/ghc-events/pull/40))

## 0.7.3 - 2018-07-10

* Fixed memory-leak in incremental readEvents ([#37](https://github.com/haskell/ghc-events/pull/37))
* Relax upper version bound for containers ([#38](https://github.com/haskell/ghc-events/pull/38))

## 0.7.2 - 2018-03-13

* Add Semigroup instance for MaxVars to build with ghc-8.4

## 0.7.1 - 2018-02-17

* Export HeapProfBreakdown, HeapProfFlags, and PID types ([#35](https://github.com/haskell/ghc-events/pull/35))

## 0.7.0 - 2017-10-04

* Add support for heap profiling events ([#29](https://github.com/haskell/ghc-events/pull/29))

## 0.6.0 - 2017-05-31

This contains breaking changes.

* The deprecation notice on `readEventLogFromFile` has been retracted
* The incremental API has been refactored

The details are as follows:

* Update bug tracker URL ([#10](https://github.com/haskell/ghc-events/pull/10))
* New test for Eden events ([#11](https://github.com/haskell/ghc-events/pull/11))
* Relax version bound for binary ([#15](https://github.com/haskell/ghc-events/pull/15))
* Enable Travis CI ([#19](https://github.com/haskell/ghc-events/pull/19))
* Refactor the incremental API which was introduced in 0.5.0.0 ([#22](https://github.com/haskell/ghc-events/pull/22))
* Some speed/memory usage improvements ([#18](https://github.com/haskell/ghc-events/pull/18), [#22](https://github.com/haskell/ghc-events/pull/22))

## 0.5.0.0 - unreleased

* Readme added :)
* Old parser replaced with an incremental implementation
* General overhaul of the codebase
* Partial Haddock coverage

The 0.5.* releases should be able to handle large event logs and logs that have been cut off abruptly, e.g. from executable runs that resulted in crashes.

This release should be *mostly* backwards compatible, however the "old" way of reading event logs, namely the `readEventLogFromFile`  function is now **deprecated**.

**NOTE:** Users parsing large logs may notice that this version of the library is noticably slower than the older versions. The incremental parser is at fault here - previous versions of the libray used a "trick" that would let them essentially skip the first step of the mergesort algorithm since `EventBlock`s were already sorted in time order. The new parser reads the file incrementally and cannot take the advantage of this. Apologies for any inconvenience this may have caused.
