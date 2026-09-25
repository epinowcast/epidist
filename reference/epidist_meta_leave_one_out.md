# Refit a meta model leaving each study out in turn

Refits a meta model once per study with that study removed, and compares
the delay mean and standard deviation of each refit with the full fit.

## Usage

``` r
epidist_meta_leave_one_out(
  fit,
  data = NULL,
  newdata = NULL,
  re_formula = NA,
  width = 0.95,
  keep_fits = FALSE,
  ...
)
```

## Arguments

- fit:

  A meta model fitted with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) to
  an `epidist_meta_model` object.

- data:

  The `epidist_meta_model` object `fit` was fitted to. Only needed when
  `fit$data` has no `study` column, which happens when the formula does
  not use `study`. If `NULL`, the default, the model data stored in
  `fit` is used.

- newdata:

  A `data.frame` of data to predict the delay for, passed to
  [`delay_summary_draws()`](https://epidist.epinowcast.org/reference/delay_summary_draws.md).
  If `NULL`, the default, a single row giving the population level delay
  with no censoring and no truncation is built with
  [`epidist_newdata()`](https://epidist.epinowcast.org/reference/epidist_newdata.md).

- re_formula:

  Passed to
  [`brms::prepare_predictions()`](https://paulbuerkner.com/brms/reference/prepare_predictions.html)
  through
  [`delay_summary_draws()`](https://epidist.epinowcast.org/reference/delay_summary_draws.md).
  The default `NA` switches off any study level term, so the summaries
  are of the population level delay.

- width:

  The width of the central posterior intervals. Defaults to `0.95`.

- keep_fits:

  If `TRUE`, the refits are returned in the `fits` attribute of the
  result as a list named by the held out study. Defaults to `FALSE`.

- ...:

  Additional arguments passed to
  [`brms::update.brmsfit()`](https://paulbuerkner.com/brms/reference/update.brmsfit.html)
  and so to
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html),
  such as `cores`, `chains`, `iter`, `refresh` and `silent`.

## Value

A `tibble` with one row per held out study, row of `newdata` and summary
(`"mean"` or `"sd"`). It gives the posterior median (`estimate`) and
interval (`lower`, `upper`) of the refit and of the full fit (`full_*`),
and the `difference` from the full fit with its interval. Predictors of
the model in `newdata` other than `study` are included. The `width`
attribute records the interval width.

## Details

Individual level rows are held out together as the `"individual"` study.
If the formula does not use `study`, pass the `epidist_meta_model`
object as `data`, because `brms` drops unused variables from `fit$data`.
Each refit reuses the compiled model through
[`brms::update.brmsfit()`](https://paulbuerkner.com/brms/reference/update.brmsfit.html).
The refits are exact because Pareto smoothed importance sampling
(Vehtari et al. 2017) is often unreliable when a whole study is left
out.

The comparison is reported as estimates with intervals and no threshold,
as in `marginaleffects` (Arel-Bundock et al. 2024). `difference` is the
refit posterior minus the full fit's posterior median. The two fits
share most of their data, so their draws are not differenced.

## References

- Vehtari et al. (2017)
  [doi:10.1007/s11222-016-9696-4](https://doi.org/10.1007/s11222-016-9696-4)

- Arel-Bundock et al. (2024)
  [doi:10.18637/jss.v111.i09](https://doi.org/10.18637/jss.v111.i09)

## See also

[`delay_summary_draws()`](https://epidist.epinowcast.org/reference/delay_summary_draws.md)
for the summaries the comparison uses and
[`epidist_newdata()`](https://epidist.epinowcast.org/reference/epidist_newdata.md)
to build `newdata`.

Other meta_model:
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md),
[`as_epidist_meta_model.NULL()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.NULL.md),
[`as_epidist_meta_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_aggregate_data.md),
[`as_epidist_meta_model.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_estimates_data.md),
[`as_epidist_meta_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_linelist_data.md),
[`assert_epidist.epidist_meta_model()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_meta_model.md),
[`epidist_family_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.epidist_meta_model.md),
[`epidist_formula_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_meta_model.md),
[`epidist_model_prior.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_model_prior.epidist_meta_model.md),
[`epidist_newdata.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_meta_model.md),
[`epidist_transform_data_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_meta_model.md),
[`is_epidist_meta_model()`](https://epidist.epinowcast.org/reference/is_epidist_meta_model.md),
[`new_epidist_meta_model()`](https://epidist.epinowcast.org/reference/new_epidist_meta_model.md)

## Examples

``` r
# \donttest{
estimates <- as_epidist_estimates_data(
  data.frame(
    study = c("A", "A", "B", "B", "C", "C"),
    type = c("mean", "sd", "mean", "sd", "mean", "sd"),
    value = c(7.5, 3.6, 6.4, 3.1, 8.2, 4.0),
    n = c(120, 120, 80, 80, 150, 150),
    relative_obs_time = c(20, 20, 25, 25, Inf, Inf),
    trunc_adjusted = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
    cens_adjusted = c(0, 0, 0, 0, 1, 1)
  )
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
meta <- as_epidist_meta_model(estimates = estimates)
fit <- epidist(
  meta,
  chains = 2, cores = 2, refresh = ifelse(interactive(), 250, 0)
)
#> Compiling Stan program...
#> Start sampling

# The formula does not use study, so pass the model data
epidist_meta_leave_one_out(fit, data = meta, refresh = 0)
#> Start sampling
#> Start sampling
#> Start sampling
#> # A tibble: 6 × 12
#>   study  .row summary estimate lower upper full_estimate full_lower full_upper
#>   <chr> <int> <chr>      <dbl> <dbl> <dbl>         <dbl>      <dbl>      <dbl>
#> 1 A         1 mean        7.75  7.26  8.32          7.75       7.34       8.27
#> 2 A         1 sd          4.03  3.43  5.05          4.09       3.55       4.96
#> 3 B         1 mean        8.06  7.56  8.73          7.75       7.34       8.27
#> 4 B         1 sd          4.22  3.59  5.39          4.09       3.55       4.96
#> 5 C         1 mean        7.28  6.74  7.97          7.75       7.34       8.27
#> 6 C         1 sd          3.87  3.18  4.85          4.09       3.55       4.96
#> # ℹ 3 more variables: difference <dbl>, difference_lower <dbl>,
#> #   difference_upper <dbl>
# }
```
