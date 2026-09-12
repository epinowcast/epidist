# Posterior draws of the delay distribution parameters

Returns posterior draws of the parameters of the delay distribution in
the long format used by `tidybayes`. The delay parameters are the
distributional parameters of the `brms` family, evaluated on the
response scale for each row of `newdata`. For a lognormal model they are
`mu` and `sigma`. They are the parameters of the delay distribution
itself, so they do not describe the censoring or truncation of the
observation process, and they are not the natural scale mean and
standard deviation of the delay. Use
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md)
to add those.

`add_delay_parameter_draws()` is the same function with `newdata` first,
for use at the start of a pipeline as with
[`tidybayes::add_epred_draws()`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html).

## Usage

``` r
delay_parameter_draws(object, newdata = NULL, ...)

add_delay_parameter_draws(newdata, object, ...)
```

## Arguments

- object:

  A model fit with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

- newdata:

  A `data.frame` of data to predict for. If `NULL`, the default, the
  data the model was fitted to is used. The `brms` models `epidist` fits
  need the model variables as well as the predictors, so build `newdata`
  with
  [`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)
  rather than from the predictors alone.

- ...:

  Additional arguments passed to
  [`brms::prepare_predictions()`](https://paulbuerkner.com/brms/reference/prepare_predictions.html),
  such as `ndraws` or `re_formula`.

## Value

A `tibble` of posterior draws of the delay distribution parameters,
grouped by the columns of `newdata` and by `.row`. It has the
`epidist_delay_draws` class, which records the delay distribution family
and gives it a
[plot()](https://epidist.epinowcast.org/reference/plot.epidist_delay_draws.md)
method.

## Details

The returned columns follow the `tidybayes` conventions. The columns of
`newdata` come first, followed by `.row`, `.chain`, `.iteration` and
`.draw`, followed by one column per distributional parameter. The result
is grouped by the columns of `newdata` and by `.row`. `.chain` and
`.iteration` are `NA` when the draws have been subset, because the chain
a subset draw came from is not recoverable.

Every row of `newdata` gets its own draws, so passing the data the model
was fitted to produces many identical draws when the model has few
unique combinations of predictors.
[`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)
returns one row per unique combination and is usually the better input.

## See also

[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md)
to add natural scale summaries of the delay,
[`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)
to build `newdata`, and
[`plot.epidist_delay_draws()`](https://epidist.epinowcast.org/reference/plot.epidist_delay_draws.md)
to plot the draws.

Other postprocess:
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md),
[`delay_summary_draws()`](https://epidist.epinowcast.org/reference/delay_summary_draws.md),
[`epidist_delay_draws`](https://epidist.epinowcast.org/reference/epidist_delay_draws.md),
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

fit |>
  epidist_strata() |>
  add_delay_parameter_draws(fit) |>
  add_summaries(probs = c(0.05, 0.95))
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
