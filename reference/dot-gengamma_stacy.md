# Convert the generalised gamma from the Prentice to the Stacy form

`primarycensored` and
[`flexsurv::dgengamma.orig()`](http://chjackson.github.io/flexsurv-dev/reference/GenGamma.orig.md)
use the Stacy (1962) form with `shape`, `scale` and `k`. It has no
counterpart for `Q` of zero or below, which is why
[`gengamma()`](https://epidist.epinowcast.org/reference/gengamma.md)
keeps `Q` positive.

## Usage

``` r
.gengamma_stacy(mu, sigma, Q)
```

## Arguments

- mu, sigma, Q:

  Parameters of
  [`flexsurv::dgengamma()`](http://chjackson.github.io/flexsurv-dev/reference/GenGamma.md).

## Value

A named list of `shape`, `scale` and `k`.
