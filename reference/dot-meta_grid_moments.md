# Summaries of a discrete delay grid

Summaries of a discrete delay grid

## Usage

``` r
.meta_grid_moments(mass, first_delay, swindow)
```

## Arguments

- mass:

  A vector of grid probabilities from
  [`.meta_grid_pmf()`](https://epidist.epinowcast.org/reference/dot-meta_grid_pmf.md),
  or a vector of `NA` if the grid mass underflowed to zero.

- first_delay:

  The delay the first kept cell records.

- swindow:

  The secondary censoring window width, which is also the grid spacing.

## Value

A named numeric vector with elements `mean`, `sd`, `kurtosis` and
`skewness`.
