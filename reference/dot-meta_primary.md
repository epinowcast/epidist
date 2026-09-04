# The primary event distribution implied by a growth rate

A growth rate of zero corresponds to a uniform primary event within its
censoring window. Any other value uses the exponential growth primary
distribution from `primarycensored`.

## Usage

``` r
.meta_primary(growth_rate)
```

## Arguments

- growth_rate:

  The exponential growth rate of primary events.

## Value

A list with elements `dprimary` and `dprimary_args`.
