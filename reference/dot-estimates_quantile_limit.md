# The largest reported quantile a set of summary estimates can support

A quantile reported at or beyond the top of the estimand's support has
an implied cumulative probability of one and an implied density of zero,
so the delta method conversion of a delay scale standard error hits its
floor and the row contributes a constant to the likelihood instead of
information. The limit is the top of the discrete grid for a study that
did not adjust for censoring, allowing for the half cell the continuity
correction adds and for the half window midpoint imputation shifts by,
and the grid cutoff otherwise.

## Usage

``` r
.estimates_quantile_limit(data)
```

## Arguments

- data:

  An `epidist_estimates_data` object.

## Value

A numeric vector of limits, one per row.
