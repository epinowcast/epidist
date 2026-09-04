# The density function used for a `primarycensored` distribution name

Shares the distribution function lookup with
[`.pdist()`](https://epidist.epinowcast.org/reference/dot-pdist.md) in
`R/gen.R`; only the density direction is meta model specific.

## Usage

``` r
.meta_ddist(dist)
```

## Arguments

- dist:

  A `primarycensored` distribution function name, for example
  `"plnorm"`.

## Value

The corresponding function from `stats`.
