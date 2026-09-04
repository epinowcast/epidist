# The meta model method for `epidist_aggregate_data` objects

This method converts aggregate data to a meta model format by passing it
to
[`as_epidist_meta_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_linelist_data.md)
with the `n` column used as weights.

## Usage

``` r
# S3 method for class 'epidist_aggregate_data'
as_epidist_meta_model(
  data,
  estimates = NULL,
  obs_time_threshold = 2,
  delay_min = NULL,
  primary = .primary_choices(),
  ...
)
```

## Arguments

- data:

  An `epidist_aggregate_data` object.

- estimates:

  An `epidist_estimates_data` object of published summary estimates, or
  `NULL`.

- obs_time_threshold:

  Ratio used to determine threshold for setting relative observation
  times to Inf. Observation times greater than `obs_time_threshold`
  times the maximum delay will be set to Inf to improve model efficiency
  by reducing the number of unique observation times. Default is 2.

- delay_min:

  Minimum delay (left truncation point). Can be:

  - `NULL` (default): uses a `delay_min` column from the data if
    present, otherwise defaults to 0 (no left truncation).

  - A numeric scalar: applied to all observations.

  - A column name string: looks up the named column in the data. This is
    passed as the `L` parameter to
    [`primarycensored::dpcens()`](https://primarycensored.epinowcast.org/reference/dprimarycensored.html).

- primary:

  The distribution of the primary event within its censoring window for
  the individual level rows, as in
  [`as_epidist_marginal_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.epidist_linelist_data.md).
  `"uniform"`, the default, assumes it is equally likely at any point.
  `"expgrowth"` tilts it, with the growth rate estimated as the
  `pgrowth` distributional parameter. Summary rows are unaffected. They
  tilt the primary event by the `growth_rate` metadata of their study,
  which
  [`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
  takes as a known quantity.

- ...:

  Additional arguments passed to methods.

## See also

Other meta_model:
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md),
[`as_epidist_meta_model.NULL()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.NULL.md),
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
sierra_leone_ebola_data |>
  dplyr::count(date_of_symptom_onset, date_of_sample_tested) |>
  as_epidist_aggregate_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested",
    n = "n"
  ) |>
  as_epidist_meta_model()
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
#> # A tibble: 2,453 × 30
#>    delay_lwr     n obs_type study_n trunc_adjusted trunc_design cens_adjusted
#>        <int> <int>    <int>   <int>          <int>        <int>         <int>
#>  1         5     1        1       0              0            0             0
#>  2         5     2        1       0              0            0             0
#>  3         5     4        1       0              0            0             0
#>  4         5     6        1       0              0            0             0
#>  5         5     1        1       0              0            0             0
#>  6         5     3        1       0              0            0             0
#>  7         5     7        1       0              0            0             0
#>  8         5     7        1       0              0            0             0
#>  9         5     1        1       0              0            0             0
#> 10         7     1        1       0              0            0             0
#> # ℹ 2,443 more rows
#> # ℹ 23 more variables: group_start <int>, group_len <int>, chol_start <int>,
#> #   n_quad <int>, relative_obs_time <dbl>, pwindow <dbl>, swindow <dbl>,
#> #   delay_upr <dbl>, delay_min <dbl>, report_se <dbl>, quantile_p <dbl>,
#> #   growth_rate <dbl>, ptime_lwr <dbl>, ptime_upr <dbl>, stime_lwr <dbl>,
#> #   stime_upr <dbl>, obs_time <dbl>, pdate_lwr <date>, sdate_lwr <date>,
#> #   pdate_upr <date>, sdate_upr <date>, obs_date <date>, …
```
