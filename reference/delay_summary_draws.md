# Posterior draws of the delay distribution, summarised

The usual last step of a fit. Builds one row per unique combination of
the predictors with
[`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md),
draws the delay distribution parameters for each with
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md),
and adds the natural scale mean and standard deviation, and any
quantiles asked for, with
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md).

Each of those three steps is available on its own where a step needs
different arguments, or where `newdata` is built some other way.

## Usage

``` r
delay_summary_draws(
  object,
  newdata = NULL,
  vars = NULL,
  probs = NULL,
  method = c("auto", "analytic", "sample"),
  nsim = 1000,
  ...
)
```

## Arguments

- object:

  A model fit with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

- newdata:

  A `data.frame` of data to predict for. If `NULL`, the default,
  [`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)
  builds one row per unique combination of the predictors the model
  uses.

- vars:

  A character vector of variables to stratify by, passed to
  [`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md).
  Only used when `newdata` is `NULL`.

- probs:

  A numeric vector of probabilities to add quantiles of the delay
  distribution for. If `NULL`, the default, no quantiles are added.

- method:

  Passed to
  [`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md).
  Either `"auto"`, the default, which uses the analytic solution when
  there is one and simulates otherwise, `"analytic"`, or `"sample"`.

- nsim:

  The number of delays to simulate per row. Passed to
  [`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md)
  and only used when simulating.

- ...:

  Additional arguments passed to
  [`brms::prepare_predictions()`](https://paulbuerkner.com/brms/reference/prepare_predictions.html),
  such as `ndraws` or `re_formula`.

## Value

A `tibble` of posterior draws of the delay distribution parameters with
`mean` and `sd` columns added, and one column per element of `probs`.
Grouped as
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)
returns it.

## See also

[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)
for the parameters alone,
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md)
to summarise draws you already have, and
[`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)
to build `newdata`.

Other postprocess:
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md),
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md),
[`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)

## Examples

``` r
# \donttest{
fit <- sierra_leone_ebola_data |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ) |>
  as_epidist_aggregate_data() |>
  as_epidist_marginal_model() |>
  epidist(chains = 2, cores = 2, refresh = ifelse(interactive(), 250, 0))
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).
#> ! Setting 2394 relative observation times (`relative_obs_time`) greater than 98
#>   (2x the maximum delay) to Inf.
#> ℹ This improves model efficiency by reducing the number of unique observation
#>   times in the data.
#> ℹ The impact on model accuracy should be negligible because these relative
#>   observation times are high enough to cause very limited right truncation.
#> ℹ The original relative observation times are available in
#>   `orig_relative_obs_time`.
#> ℹ Raise `obs_time_threshold` to avoid this behaviour.
#> Warning: Found infinite values in the data, which may cause issues for Stan.
#> ℹ Data summarised by unique combinations of:
#> * Model variables: delay bounds, observation time, and primary censoring window
#> ! Reduced from 2453 to 272 rows.
#> ℹ This should improve model efficiency with no loss of information.
#> Warning: Found infinite values in the data, which may cause issues for Stan.
#> Warning: Found infinite values in the data, which may cause issues for Stan.
#> Compiling Stan program...
#> Start sampling

delay_summary_draws(fit, probs = c(0.05, 0.95))
#> Warning: Found infinite values in the data, which may cause issues for Stan.
#> # A tibble: 2,000 × 17
#> # Groups:   delay_lwr, relative_obs_time, pwindow, swindow, delay_upr,
#> #   delay_min, n, .row [1]
#>    delay_lwr relative_obs_time pwindow swindow delay_upr delay_min     n  .row
#>        <dbl>             <dbl>   <dbl>   <dbl>     <dbl>     <dbl> <int> <int>
#>  1         5               Inf       1       1         6         0    50     1
#>  2         5               Inf       1       1         6         0    50     1
#>  3         5               Inf       1       1         6         0    50     1
#>  4         5               Inf       1       1         6         0    50     1
#>  5         5               Inf       1       1         6         0    50     1
#>  6         5               Inf       1       1         6         0    50     1
#>  7         5               Inf       1       1         6         0    50     1
#>  8         5               Inf       1       1         6         0    50     1
#>  9         5               Inf       1       1         6         0    50     1
#> 10         5               Inf       1       1         6         0    50     1
#> # ℹ 1,990 more rows
#> # ℹ 9 more variables: .chain <int>, .iteration <int>, .draw <int>, mu <dbl>,
#> #   sigma <dbl>, mean <dbl>, sd <dbl>, q5 <dbl>, q95 <dbl>
# }
```
