# Deep lower tail bounds of the delay distributions

One per family with a closed form bound, see
[`.meta_deep_tail()`](https://epidist.epinowcast.org/reference/dot-meta_deep_tail.md).
They mirror the Stan functions of the same name in
`inst/stan/meta_model/functions.stan`.

## Usage

``` r
.meta_deep_tail_lognormal(q, args)

.meta_deep_tail_gamma(q, args)

.meta_deep_tail_weibull(q, args)

.meta_deep_tail_gengamma(q, args)
```

## Arguments

- q:

  A numeric vector of positive delays.

- args:

  A named list of distribution parameters.

## Value

A logical vector.
