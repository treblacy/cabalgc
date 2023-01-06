# Revision history for cabalgc

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
