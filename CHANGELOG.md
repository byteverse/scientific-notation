# Revision history for scientific-notation

## 0.1.6.0 -- 2023-??-??O

* Support GHC 9.4 and 9.6. Drop support for GHCs older than 9.4.

## 0.1.5.0 -- 2022-07-15

* Support GHC 9.2.

## 0.1.4.0 -- 2022-03-16

* Add `fromWord8`, `fromWord16`, and `fromWord32`.
* Add `roundShiftedToInt64`.
* Add `encode`.
* Change `builderUtf8` to present numbers in decimal notation without exponent
  in many common cases. This is not considered a breaking change.

## 0.1.3.0 -- 2021-02-23

* Add `greaterThanInt64`.
* Add `fromWord64`.
* Suppress zero exponent when encoding.

## 0.1.2.0 -- 2020-05-01

* Add `builderUtf8`.

## 0.1.1.0 -- 2019-11-06

* Add `withExposed`.
* Start building with `-O2`.

## 0.1.0.1 -- 2019-09-30

* Make compatible with `bytesmith-0.3`.

## 0.1.0.0 -- 2019-09-24

* First version. Released on an unsuspecting world.
