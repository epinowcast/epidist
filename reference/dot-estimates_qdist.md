# The quantile function used for a `primarycensored` distribution name

Shares the distribution function lookup with
[`.pdist()`](https://epidist.epinowcast.org/reference/dot-pdist.md) in
`R/gen.R`; only the quantile direction is specific to reported
distribution parameters.

## Usage

``` r
.estimates_qdist(dist)
```

## Arguments

- dist:

  A `primarycensored` distribution function name, for example
  `"plnorm"`.

## Value

The corresponding function from `stats`.
