# The summaries implied by one meta model row for one draw

The summaries implied by one meta model row for one draw

## Usage

``` r
.meta_row_moments(slots, dist, args)
```

## Arguments

- slots:

  The output of
  [`.meta_row_slots()`](https://epidist.epinowcast.org/reference/dot-meta_row_slots.md).

- dist:

  A `primarycensored` distribution function name.

- args:

  A named list of distribution parameters for a single draw.

## Value

A named numeric vector with elements `mean`, `sd`, `kurtosis` and
`skewness`.
