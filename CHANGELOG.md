# Change Log

## Unreleased
* Drop dependency on `array` ([#114](https://github.com/haskell/ghc-events/pull/114))
* Make definition of `Monoid MaxVars` cannonical ([#115](https://github.com/haskell/ghc-events/pull/115))


## 0.20.0.0 - 2024-11-25

* Add pretty printing for timestamps ([#92](https://github.com/haskell/ghc-events/pull/92))
* Added `--version` option to CLI interface ([#95](https://github.com/haskell/ghc-events/pull/95))
* Support GHC 9.8 ([#102](https://github.com/haskell/ghc-events/pull/102))
* Add support for era profiling events ([#103](https://github.com/haskell/ghc-events/pull/103))
* Support GHC 9.10 ([#105](https://github.com/haskell/ghc-events/pull/105))
* Add support for the NonmovingPrunedSegment event ([#107](https://github.com/haskell/ghc-events/pull/107))
* Fix the type of the `profCap` field of `ProfSampleCostCentre` ([#108](https://github.com/haskell/ghc-events/pull/108))


## 0.19.0.1 - 2023-04-13

* Update for GHC 9.6 ([#93](https://github.com/haskell/ghc-events/pull/93))

## 0.19.0 - 2022-12-15

* Add support for extension to Ticky counter definition field ([#83](https://github.com/haskell/ghc-events/pull/83))
* Add support for ticky definition json fields ([#87](https://github.com/haskell/ghc-events/pull/87))

## 0.18.0 - 2022-10-28

* Ensure that ghc-events show fails with an error on malformed events ([#86](https://github.com/haskell/ghc-events/pull/86))
* Drop support for GHC < 8 ([#89](https://github.com/haskell/ghc-events/issues/89))
* Allow parsing UserBinaryMessage events that have non-utf payloads ([#91](https://github.com/haskell/ghc-events/pull/91))
* Update dependencies to support GHC 9.4

## 0.17.0.3 - 2022-04-18

* Fix typos and terminology ([#81](https://github.com/haskell/ghc-events/pull/81), [#82](https://github.com/haskell/ghc-events/pull/82))

## 0.17.0.2 - 2022-02-14

* Clarify usage of IntMap EventType in ppEvent ([#80](https://github.com/haskell/ghc-events/pull/80))
* Relax upper version bound for text

## 0.17.0.1 - 2021-11-22

* Relax base bound to support GHC-9.2.1 ([#78](https://github.com/haskell/ghc-events/pull/78))

## 0.17.0 - 2021-05-06

* Introduce EVENT_TICKY_BEGIN_SAMPLE ([#76](https://github.com/haskell/ghc-events/pull/76))

## 0.16.0 - 2021-03-12

* Add support for new 9.2 events ([#74](https://github.com/haskell/ghc-events/pull/74))

## 0.15.1 - 2020-12-30

* Add missing extra-source-files ([#71](https://github.com/haskell/ghc-events/pull/71))

## 0.15.0 - 2020-12-16

* Add support for ticky-ticky counts ([#67](https://github.com/haskell/ghc-events/pull/67))

## 0.14.0 - 2020-11-17

* Add support for non-moving GC events ([#60](https://github.com/haskell/ghc-events/pull/60))
* Fix the parser error under GHC 9.0 ([#64](https://github.com/haskell/ghc-events/pull/64))
* Fix string encodings ([#62](https://github.com/haskell/ghc-events/pull/62))
* Switch to GitHub Actions ([#65](https://github.com/haskell/ghc-events/pull/65)), dropping GHC 7.8.4 from the support range

## 0.13.0 - 2020-04-04

* Fix broken UTF-8 decoding ([#55](https://github.com/haskell/ghc-events/pull/55))
  * This is a breaking change. Most of the String fields in EventInfo have been replaced with Texts.
* Support GHC 8.10.1

## 0.12.0 - 2019-12-02

* Add support for EVENT_USER_BINARY_MSG ([#54](https://github.com/haskell/ghc-events/pull/54))

## 0.11.0 - 2019-10-29

* Add support for time profiling events (ProfSampleCostCentre and ProfBegin) ([#53](https://github.com/haskell/ghc-events/pull/53))

## 0.10.0 - 2019-10-01

* Add support for HeapProfSampleEnd and HeapBioProfSampleBegin ([#52](https://github.com/haskell/ghc-events/pull/52))

## 0.9.1 - 2019-09-03

* Relax upper version bounds to support GHC 8.8.1 ([#49](https://github.com/haskell/ghc-events/pull/49) and [#51](https://github.com/haskell/ghc-events/pull/51))

## 0.9.0 - 2019-05-15

* Support the newly added par_balanced_copied field ([#47](https://github.com/haskell/ghc-events/pull/47))

## 0.8.0.2 - 2019-04-02

* Tighten lower version bound for base ([#46](https://github.com/haskell/ghc-events/pull/46))

## 0.8.0.1 - 2018-10-22

* Relax upper version bound for base to support GHC 8.6
* Relax upper version bound for binary

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

**NOTE:** Users parsing large logs may notice that this version of the library is noticeably slower than the older versions. The incremental parser is at fault here - previous versions of the library used a "trick" that would let them essentially skip the first step of the mergesort algorithm since `EventBlock`s were already sorted in time order. The new parser reads the file incrementally and cannot take the advantage of this. Apologies for any inconvenience this may have caused.
