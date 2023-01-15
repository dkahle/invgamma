# invgamma 1.2

## New features

* **invgamma** has an updated README containing a simulation check on `rinvgamma()`.
  
## Changes

* `rinvgamma()` now errors for `shape` parameter less than 0.01, as it is unreliable there.
* **invgamma** now cautions users about the parameterizations used in `(d/p/q/r)invgamma()` functions.
* **invgamma** now has a MIT license.


# invgamma 1.1

## New features

* `(d/p/q/r)invgamma()` now defaults the rate parameter to 1, like `(d/p/q/r)gamma()`.
  
## Fixes

* README images moved from figures/ to tools/





# invgamma 1.0

## New features

* `(d/p/q/r)invchisq()` and `(d/p/q/r)invexp()` are now included, tied to their base R counterparts.
  



# invgamma 1.0

## New features

* `dinvgamma()` is now tied to `dgamma()` in base, instead of being a custom implementation of the density.
  
