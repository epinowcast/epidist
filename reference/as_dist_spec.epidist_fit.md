# Export a fitted delay distribution as a `distspec` distribution

Summarises the posterior of a fitted delay distribution into an
uncertain `<dist_spec>` from the `distspec` package, for use in packages
that take their delay distributions in that form. The natural parameters
of the delay distribution are computed for each posterior draw and
summarised into a prior on each of them.

## Usage

``` r
# S3 method for class 'epidist_fit'
as_dist_spec(x, newdata = NULL, max = Inf, cdf_max = 1, ...)
```

## Arguments

- x:

  A model fit with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

- newdata:

  A `data.frame` of data to predict for, with one row per delay
  distribution wanted. If `NULL`, the default,
  [`epidist_newdata()`](https://epidist.epinowcast.org/reference/epidist_newdata.md)
  builds one row per unique combination of the predictors, with no
  censoring and no truncation. See the details.

- max:

  The maximum of the delay distribution, passed to
  [`distspec::bound_dist()`](https://epiforecasts.io/distspec/reference/bound_dist.html).
  Defaults to `Inf`, which is no maximum.

- cdf_max:

  The cumulative probability to keep the delay distribution up to,
  passed to
  [`distspec::bound_dist()`](https://epiforecasts.io/distspec/reference/bound_dist.html).
  Defaults to 1, which keeps the whole distribution.

- ...:

  Additional arguments passed to
  [`brms::prepare_predictions()`](https://paulbuerkner.com/brms/reference/prepare_predictions.html),
  such as `ndraws` or `re_formula`.

## Value

A `<dist_spec>` when `newdata` has one row. A named list of them, one
per row of `newdata`, when it has several. The names give the values of
the columns of `newdata` that differ between rows, such as `"sex=0"`,
and are the row numbers when no column differs.

## Details

The lognormal, gamma and Weibull families are supported, as those are
the delay distributions `distspec` has. Their `brms` parameters are
mapped to the natural parameters of the matching `distspec` constructor
for every draw before summarising:

- lognormal: `mu` is `meanlog` and `sigma` is `sdlog` of
  [`distspec::LogNormal()`](https://epiforecasts.io/distspec/reference/LogNormal.html).

- gamma: `shape` is `shape` and `shape / mu` is `rate` of
  [`distspec::Gamma()`](https://epiforecasts.io/distspec/reference/Gamma.html).

- Weibull: `shape` is `shape` and `mu / gamma(1 + 1 / shape)` is `scale`
  of
  [`distspec::Weibull()`](https://epiforecasts.io/distspec/reference/Weibull.html).

Open an issue at <https://github.com/epiforecasts/distspec/issues> to
ask `distspec` for another distribution.

Each natural parameter gets a
[`distspec::Normal()`](https://epiforecasts.io/distspec/reference/Normal.html)
prior with the mean and standard deviation of its marginal posterior.
The posterior correlation between the parameters is not represented, so
sampling from the result gives a wider range of delay distributions than
the posterior does.

The default `newdata` is built with
[`epidist_newdata()`](https://epidist.epinowcast.org/reference/epidist_newdata.md)
by expanding the variables in the model formula into a grid, so it has
one row per unique combination of the predictors, and gives the delay
distribution with no censoring and no truncation. A model with only an
intercept gets a single row. A continuous predictor gets a row per
distinct value, so pass `newdata` for such a model.

## See also

[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)
for the draws this summarises and
[`epidist_newdata()`](https://epidist.epinowcast.org/reference/epidist_newdata.md)
to build `newdata`.

Other postprocess:
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md),
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md),
[`delay_summary_draws()`](https://epidist.epinowcast.org/reference/delay_summary_draws.md),
[`epidist_delay_draws`](https://epidist.epinowcast.org/reference/epidist_delay_draws.md),
[`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)

## Examples

``` r
# \donttest{
if (requireNamespace("distspec", quietly = TRUE)) {
  fit <- sierra_leone_ebola_data |>
    as_epidist_linelist_data(
      pdate_lwr = "date_of_symptom_onset",
      sdate_lwr = "date_of_sample_tested"
    ) |>
    as_epidist_aggregate_data() |>
    as_epidist_marginal_model() |>
    epidist(chains = 2, cores = 2, refresh = ifelse(interactive(), 250, 0))

  dist <- distspec::as_dist_spec(fit)
  dist

  # The delay distribution at the posterior mean of its parameters
  distspec::fix_parameters(dist, strategy = "mean")

  # Bound the delay distribution at 60 days for a package that takes bounds
  distspec::as_dist_spec(fit, max = 60)
}
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
#> - lognormal distribution
#> Returning NA: this distribution has uncertain parameters.
#> ℹ Resolve the uncertainty first with `fix_parameters()`.
#> This message is displayed once every 8 hours.
#>  (max: 60):
#>   meanlog:
#>     - normal distribution:
#>       mean:
#>         1.6
#>       sd:
#>         0.0066
#>   sdlog:
#>     - normal distribution:
#>       mean:
#>         0.59
#>       sd:
#>         0.0048
# }
```
