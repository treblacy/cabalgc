# Revision history for cabalgc

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
