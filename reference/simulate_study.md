# Simulate the summaries a published study would have reported

Applies the observation and estimation procedure of one study to a
simulated line list and returns the summaries that study would have
published, with the metadata
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
needs to forward model them. The line list must carry the exact event
times that
[`simulate_dates()`](https://epidist.epinowcast.org/reference/simulate_dates.md)
keeps when `keep_times = TRUE`, because the censoring adjustments a
study may have used differ in how much of each event time they saw.

## Usage

``` r
simulate_study(
  data,
  study,
  report = c("moments", "quantiles", "multivariate", "mean_se"),
  probs = c(0.25, 0.5, 0.75),
  cens_adjusted = 0,
  trunc_adjusted = FALSE,
  trunc_design = c("cohort", "accrual"),
  relative_obs_time = Inf,
  delay_min = 0,
  growth_rate = 0,
  n = NULL,
  max_delay = NULL,
  ...
)
```

## Arguments

- data:

  An `epidist_linelist_data` object built from simulated event times,
  with the exact `ptime` and `stime` columns kept by
  [`simulate_dates()`](https://epidist.epinowcast.org/reference/simulate_dates.md)
  when `keep_times = TRUE`. Every case must use the same primary window
  and the same secondary window.

- study:

  A string labelling the study.

- report:

  What the study published. `"moments"` gives a mean and a standard
  deviation with the sample size, `"quantiles"` the quantiles at `probs`
  with the sample size, `"multivariate"` the mean and standard deviation
  with their bootstrap covariance, through
  [`new_epidist_multivariate()`](https://epidist.epinowcast.org/reference/new_epidist_multivariate.md),
  and `"mean_se"` a mean with its standard error and no sample size.

- probs:

  The probabilities of the quantiles a `"quantiles"` study reports.

- cens_adjusted:

  The censoring adjustment code the study used, one of 0 to 4 as
  described above and in
  [`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md).

- trunc_adjusted:

  Whether the study corrected for right truncation. If `FALSE`, the
  study's truncation is applied to the line list.

- trunc_design:

  How the study stopped collecting, `"cohort"` or `"accrual"`. Only used
  when `trunc_adjusted` is `FALSE`.

- relative_obs_time:

  The study's observation time. For a `"cohort"` design this bounds each
  delay, and for an `"accrual"` design it is the length of the
  collection window from the start of the line list. Defaults to `Inf`,
  which is only allowed for a study that adjusted for right truncation.

- delay_min:

  The smallest measured delay the study counted. Cases below it are
  dropped.

- growth_rate:

  The growth rate the study is described by. Passed through as metadata
  and not used to select cases, so it should be the rate the line list
  was simulated with.

- n:

  The number of cases the study sampled from those it could have seen.
  Defaults to `NULL`, meaning all of them.

- max_delay:

  The grid cutoff passed through to
  [`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md),
  or `NULL` for its default.

- ...:

  Further columns, such as covariates, added to every row of the result.

## Value

An `epidist_estimates_data` object with one row per reported summary.

## Details

The study measures each delay as its censoring adjustment code says,
drops the cases its truncation would have hidden from it, drops delays
below `delay_min`, takes a sample of `n` cases and summarises them. Each
step follows the estimand the meta model uses for that code, see
[`vignette("model")`](https://epidist.epinowcast.org/articles/model.md).

The measured delay is, by `cens_adjusted` code:

- 0, the difference between the lower edges of the secondary and primary
  windows, which is the integer date difference of a daily line list.

- 1, the exact delay between the two events.

- 2, the exact secondary time less the lower edge of the primary window,
  the uniform single interval approximation.

- 3, the date difference of code 0 plus half a secondary window,
  midpoint imputation.

- 4, the exact secondary time less the midpoint of the primary window.

A study that did not adjust for right truncation sees a case only if its
delay had completed by its observation time. Under a `"cohort"` design
the delay the observation time bounds is the underlying one, so a study
on the discrete grid (codes 0 and 3) keeps a case only if the whole
window its delay fell in is below `relative_obs_time`, and a code 4
study keeps a case if its code 2 delay is. Under an `"accrual"` design
the study stopped at a calendar date `relative_obs_time` after the start
of the line list, and keeps every case whose secondary event fell before
it.

The advisory checks of
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
do not run on the result. They run once on the combined object when
several studies are passed to
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
in a list.

## See also

Other simulate:
[`simulate_dates()`](https://epidist.epinowcast.org/reference/simulate_dates.md),
[`simulate_exponential_cases()`](https://epidist.epinowcast.org/reference/simulate_exponential_cases.md),
[`simulate_gillespie()`](https://epidist.epinowcast.org/reference/simulate_gillespie.md),
[`simulate_secondary()`](https://epidist.epinowcast.org/reference/simulate_secondary.md),
[`simulate_uniform_cases()`](https://epidist.epinowcast.org/reference/simulate_uniform_cases.md)

## Examples

``` r
linelist <- simulate_gillespie(seed = 1) |>
  simulate_secondary(meanlog = 1.8, sdlog = 0.5) |>
  simulate_dates(keep_times = TRUE) |>
  as_epidist_linelist_data()
#> ℹ No observation time column provided, using 2024-05-04 as the observation date (the maximum of the secondary event upper bound).
simulate_study(
  linelist, "naive snapshot",
  cens_adjusted = 0, trunc_adjusted = FALSE, relative_obs_time = 15
)
#> ℹ No max_delay column supplied, using the delay beyond which 1% of the second
#>   moment of a lognormal matched to each study's summaries lies (at least 10 and
#>   at most twenty times the largest reported value, in whole secondary windows)
#>   as the grid cutoff, or five times the largest reported value where nothing
#>   can be matched. Raise it if the delay has a longer tail than that, and lower
#>   it to speed up fitting.
#> # A tibble: 2 × 16
#>   study          type  value    se     n     p pwindow swindow relative_obs_time
#>   <chr>          <chr> <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>             <dbl>
#> 1 naive snapshot mean   6.41    NA  8316    NA       1       1                15
#> 2 naive snapshot sd     2.84    NA  8316    NA       1       1                15
#> # ℹ 7 more variables: trunc_adjusted <lgl>, trunc_design <chr>,
#> #   cens_adjusted <int>, delay_min <dbl>, growth_rate <dbl>, max_delay <dbl>,
#> #   mvn_id <chr>
```
