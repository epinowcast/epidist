# Create an `epidist_estimates_data` object from a multivariate representation

This is the only route by which a covariance between a study's reported
summaries reaches
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md).
The covariance comes from draws of the study's parameters, so it is one
the study could have computed, and the summaries it covers are fitted
with a multivariate normal likelihood rather than as independent
observations.

## Usage

``` r
# S3 method for class 'epidist_multivariate'
as_epidist_estimates_data(
  data,
  study,
  family = NULL,
  moments = c("mean", "sd"),
  probs = numeric(0),
  mvn_id = NULL,
  advise = TRUE,
  ...
)
```

## Arguments

- data:

  The data to convert

- study:

  A string naming the study the draws come from.

- family:

  The distribution the study fitted, one of `"lognormal"`, `"gamma"` or
  `"weibull"`, where the draws hold its natural parameters. Defaults to
  `NULL`, meaning the draws already hold reported summaries.

- moments:

  Which moments to report, any of `"mean"` and `"sd"`. Only used where
  `family` is given.

- probs:

  A numeric vector of probabilities to report quantiles at. Only used
  where `family` is given.

- mvn_id:

  A string identifying the covariance matrix. Defaults to `study`, and
  only needs setting where one study contributes more than one
  multivariate object.

- advise:

  Whether to run the advisory checks of the Checks section and message
  about the studies they flag. Defaults to `TRUE`. The list method sets
  it to `FALSE` for each element and runs the checks once on the
  combined object.

- ...:

  Study metadata, as documented in
  [`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md).

## Details

Where `family` is given, the draws hold the natural parameters of a
distribution the study fitted. Each draw is pushed through to the
summaries the fitted distribution implies, over the range of delays the
study could have seen, and the covariance is taken over those. This is
the exact version of the delta method
[`epidist_estimates_parameters()`](https://epidist.epinowcast.org/reference/epidist_estimates_parameters.md)
applies, and it needs no linearisation.

Where `family` is `NULL`, the parameters must already be quantities a
study reports. Name them `mean`, `sd`, or `q` followed by a probability,
such as `q0.25`.

Draws over more than one index point are not yet supported for fitting,
because the linear predictor would have to vary within one likelihood
observation.

## See also

Other estimates_data:
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md),
[`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md),
[`as_epidist_estimates_data.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.epidist_estimates_data.md),
[`as_epidist_estimates_data.list()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.list.md),
[`assert_epidist.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_estimates_data.md),
[`epidist_estimates_parameters()`](https://epidist.epinowcast.org/reference/epidist_estimates_parameters.md),
[`epidist_estimates_summaries()`](https://epidist.epinowcast.org/reference/epidist_estimates_summaries.md),
[`is_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/is_epidist_estimates_data.md),
[`new_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/new_epidist_estimates_data.md)

## Examples

``` r
set.seed(1)
draws <- cbind(mean = rnorm(500, 7.5, 0.3), sd = rnorm(500, 3.6, 0.2))
as_epidist_estimates_data(
  as_epidist_multivariate(draws),
  study = "site A",
  trunc_adjusted = TRUE,
  cens_adjusted = 1
)
#> ℹ No `pwindow` column supplied, assuming a censoring window of 1 (daily
#>   reporting) for every study.
#> ℹ No `swindow` column supplied, assuming a censoring window of 1 (daily
#>   reporting) for every study.
#> ℹ No relative_obs_time column supplied, assuming no observation time limit (no
#>   right truncation) for every study.
#> ℹ No max_delay column supplied, using the delay beyond which 1% of the second
#>   moment of a lognormal matched to each study's summaries lies (at least 10 and
#>   at most twenty times the largest reported value, in whole secondary windows)
#>   as the grid cutoff, or five times the largest reported value where nothing
#>   can be matched. Raise it if the delay has a longer tail than that, and lower
#>   it to speed up fitting.
#> # A tibble: 2 × 16
#>   study  type  value    se     n     p pwindow swindow relative_obs_time
#>   <chr>  <chr> <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>             <dbl>
#> 1 site A mean   7.51    NA    NA    NA       1       1               Inf
#> 2 site A sd     3.59    NA    NA    NA       1       1               Inf
#> # ℹ 7 more variables: trunc_adjusted <lgl>, trunc_design <chr>,
#> #   cens_adjusted <int>, delay_min <dbl>, growth_rate <dbl>, max_delay <dbl>,
#> #   mvn_id <chr>
```
