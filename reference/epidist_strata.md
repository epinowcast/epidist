# Unique combinations of the predictors in a model

Returns one row of the model data for each unique combination of the
variables used to predict the delay distribution parameters. Passing
this to
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)
gives one set of draws per combination rather than one per observation,
which is the same result with far fewer draws.

## Usage

``` r
epidist_strata(object, vars = NULL)
```

## Arguments

- object:

  A model fit with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

- vars:

  A character vector of variables to take unique combinations of. If
  `NULL`, the default, the variables in the distributional parameter
  formulas are used.

## Value

A `tibble` with one row per unique combination of `vars`, with the
combination columns first.

## Details

The variables are taken from the right hand side of each distributional
parameter formula. The remaining columns are kept from the first row of
the model data in which each combination occurs. This keeps the model
variables that `brms` requires in `newdata`, such as the relative
observation time and the censoring windows for the latent and marginal
models. Those variables do not enter the distributional parameters, so
the values kept do not change the draws.

A model with only an intercept has no predictors and so returns a single
row. A continuous predictor has as many combinations as it has distinct
values, so consider passing `vars` and a grid of your own instead.

## See also

Other postprocess:
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md),
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md),
[`delay_summary_draws()`](https://epidist.epinowcast.org/reference/delay_summary_draws.md)

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

epidist_strata(fit)
#> # A tibble: 1 × 7
#>   delay_lwr relative_obs_time pwindow swindow delay_upr delay_min     n
#>       <dbl>             <dbl>   <dbl>   <dbl>     <dbl>     <dbl> <int>
#> 1         5               Inf       1       1         6         0    50
# }
```
