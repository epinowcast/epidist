# Report a study that published summaries of its delays

Builds the rows
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md)
takes from the summaries a study published in wide form, one argument
per kind of summary.

## Usage

``` r
epidist_estimates_summaries(
  study,
  mean = NULL,
  sd = NULL,
  quantiles = NULL,
  probs = NULL,
  se = NULL,
  n = NULL,
  ...
)
```

## Arguments

- study:

  A string naming the study.

- mean:

  The reported mean delay. Optional.

- sd:

  The reported standard deviation of the delays. Optional.

- quantiles:

  A numeric vector of reported quantiles. Optional.

- probs:

  The probabilities of `quantiles`, in the same order. Required where
  `quantiles` is given.

- se:

  A numeric vector of the reported standard errors of the summaries,
  ordered mean, standard deviation, then quantiles, skipping any that
  was not reported. Optional.

- n:

  The number of delays the study summarised. Optional.

- ...:

  Study metadata, as documented in
  [`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md).

## Value

An `epidist_estimates_data` object.

## Details

Give the uncertainty of each summary through `se`, or the number of
delays the study summarised through `n`, which the model uses to derive
a sampling uncertainty instead. One of the two is needed for every row.

## See also

Other estimates_data:
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md),
[`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md),
[`as_epidist_estimates_data.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.epidist_estimates_data.md),
[`as_epidist_estimates_data.epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.epidist_multivariate.md),
[`as_epidist_estimates_data.list()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.list.md),
[`assert_epidist.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_estimates_data.md),
[`epidist_estimates_parameters()`](https://epidist.epinowcast.org/reference/epidist_estimates_parameters.md),
[`is_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/is_epidist_estimates_data.md),
[`new_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/new_epidist_estimates_data.md)

## Examples

``` r
epidist_estimates_summaries(
  "study A",
  mean = 7.5, sd = 3.6, n = 120,
  relative_obs_time = 20, trunc_adjusted = FALSE, cens_adjusted = 0
)
#> ℹ No `pwindow` column supplied, assuming a censoring window of 1 (daily
#>   reporting) for every study.
#> ℹ No `swindow` column supplied, assuming a censoring window of 1 (daily
#>   reporting) for every study.
#> ℹ No trunc_design column supplied, assuming every study that did not adjust for
#>   right truncation followed a cohort with a common observation time rather than
#>   accruing primary events up to a calendar collection stop.
#> ℹ No max_delay column supplied, using the delay beyond which 1% of the second
#>   moment of a lognormal matched to each study's summaries lies (at least 10 and
#>   at most twenty times the largest reported value, in whole secondary windows)
#>   as the grid cutoff, or five times the largest reported value where nothing
#>   can be matched. Raise it if the delay has a longer tail than that, and lower
#>   it to speed up fitting.
#> # A tibble: 2 × 16
#>   study   type  value    se     n     p pwindow swindow relative_obs_time
#>   <chr>   <chr> <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>             <dbl>
#> 1 study A mean    7.5    NA   120    NA       1       1                20
#> 2 study A sd      3.6    NA   120    NA       1       1                20
#> # ℹ 7 more variables: trunc_adjusted <lgl>, trunc_design <chr>,
#> #   cens_adjusted <int>, delay_min <dbl>, growth_rate <dbl>, max_delay <dbl>,
#> #   mvn_id <chr>
```
