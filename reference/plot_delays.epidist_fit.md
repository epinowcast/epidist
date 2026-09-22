# Plot the delays a fitted model predicts over the delays it was fitted to

Draws the observed delays of the data the model was fitted to as
columns, as
[`plot_delays.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/plot_delays.epidist_linelist_data.md)
does, and the delays the model predicts for the same cases over them, as
the posterior median proportion in each bin with a ribbon between two
quantiles.

## Usage

``` r
# S3 method for class 'epidist_fit'
plot_delays(
  x,
  by = NULL,
  binwidth = 1,
  ndraws = 100,
  probs = c(0.05, 0.95),
  ...
)
```

## Arguments

- x:

  An `epidist_linelist_data` or `epidist_aggregate_data` object, a named
  list of them to compare, or a model fitted with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

- by:

  A character vector of columns of the model data that define the strata
  to colour by. If `NULL`, the default, the variables in the
  distributional parameter formulas are used, as
  [`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)
  does.

- binwidth:

  The width of the delay bins, on the scale of the event times. Defaults
  to 1, the daily censoring the data usually has.

- ndraws:

  The number of posterior draws to predict from, sampled at random.
  Defaults to 100, which is enough for the median and the quantiles of a
  binned distribution and bounds the size of the prediction. Use `NULL`
  to predict from every draw.

- probs:

  A numeric vector of two probabilities giving the quantiles the ribbon
  spans. Defaults to `c(0.05, 0.95)`.

- ...:

  Passed to the method.

## Value

A `ggplot` object.

## Details

The predictions come from
[`brms::posterior_predict()`](https://paulbuerkner.com/brms/reference/posterior_predict.brmsfit.html),
so they are of the delay as it was observed, under the censoring and
truncation of each case. That makes the plot a posterior predictive
check of the observed delays rather than a picture of the delay
distribution itself, which
[`plot.epidist_delay_draws()`](https://epidist.epinowcast.org/reference/plot.epidist_delay_draws.md)
draws.

Both the columns and the predictions are proportions within each
stratum, so a bin holds the share of cases with that delay. Bins where
neither the data nor the predictive interval puts any mass are dropped,
which trims the tail of the predictive distribution.

## See also

Other plot:
[`plot.epidist_delay_draws()`](https://epidist.epinowcast.org/reference/plot.epidist_delay_draws.md),
[`plot_delays()`](https://epidist.epinowcast.org/reference/plot_delays.md),
[`plot_delays.default()`](https://epidist.epinowcast.org/reference/plot_delays.default.md),
[`plot_delays.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/plot_delays.epidist_linelist_data.md),
[`plot_delays.list()`](https://epidist.epinowcast.org/reference/plot_delays.list.md),
[`plot_events()`](https://epidist.epinowcast.org/reference/plot_events.md)

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

plot_delays(fit)

# }
```
