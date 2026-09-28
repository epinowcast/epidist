# The primary censored distribution function, guarded against underflow

Primary distributions without an analytical solution are integrated
numerically, which can return a non finite or negative cumulative
probability deep in the lower tail. Those cases carry negligible
probability and are treated as zero, matching the guard in
`inst/stan/meta_model/functions.stan`. A delay whose plain distribution
function is below the cut of
[`.meta_log_cdf_floor()`](https://epidist.epinowcast.org/reference/dot-meta_log_cdf_floor.md)
is severed to zero before the primary censored function is called, as it
is in Stan, since the primary censored distribution function is never
above the plain one.

## Usage

``` r
.meta_pcens_cdf(q, dist, args, pwindow, growth_rate)
```

## Arguments

- q:

  A numeric vector of delays.

- dist:

  A `primarycensored` distribution function name.

- args:

  A named list of distribution parameters.

- pwindow:

  The primary censoring window width.

- growth_rate:

  The exponential growth rate of primary events.

## Value

A numeric vector of cumulative probabilities.
