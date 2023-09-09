# Revision history for cabalgc

## 1.1.0.0 -- 2023-09-08

* Package ID shortening: When outputting, hashes are truncated (can be
  overridden by an option). When inputting, only unambiguous prefixes are
  required; completion is performed.

## 1.0.4.0 -- 2023-04-19

* Add: --deps and --rdeps can focus on user-selected packages:

  If at least one pkgID on command line: Selected packages and transitive
  dependencies.  (Transitive reverse dependencies for --rdeps.)

  If no pkgID on command line: All (cabal-store) packages as before.

## 1.0.3.0 -- 2023-04-17

* Add: Warnings from --remove/-x
* Switch to my own graph-theory library.
* Relax build dependencies.
  In principle including GHC 9.6.
  In practice cabal-install-parsers's constraints are blocking that,
  but you can unblock with --allow-newer=base,transformers

## 1.0.2.0 -- 2023-01-05

* Add: Option to print version number.
* Add: Option to remove only listed packages.

## 1.0.1.3 -- 2023-01-04

* Relax dependencies for GHC 9.2 and 9.4 and current hackage.
* Oh and remove a leftover spurrious line of code :)

## 1.0.1.2 -- 2021-10-22

* Relax dependency bound (of base) for building with GHC 9.0.

## 1.0.1.1 -- 2021-10-15

* Add: Very general determining of cabal store directory.

## 1.0.1.0 -- 2021-10-14

* Add: Can list dependencies, reverse dependencies, packages not depended on.
* Fix: Now fewer blank lines when reporting unrecognized options.
* Fix: cabalgc.cabal has homepage field (points to github webpage).

## 1.0.0.1 -- 2021-10-13

* Fix: Now works if some packages don't have the "depends" field.
  (Example: https://hackage.haskell.org/package/fail built with GHC >= 8.0.)

## 1.0.0.0 -- 2021-10-13

* First version. Released on an unsuspecting world.
