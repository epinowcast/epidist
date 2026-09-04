# The summary rows a study reports

The summary rows a study reports

## Usage

``` r
.study_rows(delays, report, probs)
```

## Arguments

- delays:

  The measured delays the study summarised.

- report:

  What the study published. `"moments"` gives a mean and a standard
  deviation with the sample size, `"quantiles"` the quantiles at `probs`
  with the sample size, `"multivariate"` the mean and standard deviation
  with their bootstrap covariance, through
  [`new_epidist_multivariate()`](https://epidist.epinowcast.org/reference/new_epidist_multivariate.md),
  and `"mean_se"` a mean with its standard error and no sample size.

- probs:

  The probabilities of the quantiles a `"quantiles"` study reports.

## Value

A tibble with `type`, `value`, `p`, `n` and `se` columns.
