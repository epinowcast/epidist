# Supported primary event distributions

Supported primary event distributions

## Usage

``` r
.primary_registry()
```

## Value

A named list, one entry per distribution. Each entry gives the
`primarycensored` `id` used to dispatch in Stan, the `dpars` it adds to
the family, their `links` and `bounds`, the R density `ddist` and
sampler `rdist` used in post-processing, and the `args` of those two
functions that the `dpars` supply.
