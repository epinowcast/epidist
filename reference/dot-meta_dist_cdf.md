# The distribution function of the delay, severed deep in its lower tail

The distribution function of the delay, severed deep in its lower tail

## Usage

``` r
.meta_dist_cdf(q, dist, args)
```

## Arguments

- q:

  A numeric vector of delays.

- dist:

  A `primarycensored` distribution function name.

- args:

  A named list of distribution parameters.

## Value

A numeric vector of cumulative probabilities, zero at or below a delay
of zero, where
[`.meta_deep_tail()`](https://epidist.epinowcast.org/reference/dot-meta_deep_tail.md)
holds, and below the cut of
[`.meta_log_cdf_floor()`](https://epidist.epinowcast.org/reference/dot-meta_log_cdf_floor.md).
