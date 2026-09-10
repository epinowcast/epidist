# Which summary estimates estimate their growth rate

A study whose `growth_rate` is `NA`, or which reported it with a
positive `growth_rate_sd`, takes its rate from the `pgrowth`
distributional parameter of the meta model rather than from a number.
See
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md).

## Usage

``` r
.estimates_growth_estimated(data)
```

## Arguments

- data:

  An `epidist_estimates_data` object, or a data frame with its
  `growth_rate` and `growth_rate_sd` columns.

## Value

A logical vector, one entry per row.
