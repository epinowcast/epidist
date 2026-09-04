# Build `newdata` for the meta model

The meta model holds individual level rows and summary rows in one data
frame, telling them apart by the `obs_type` slot. Prediction is for the
delay distribution itself, so this method builds an individual level
row. It takes the same arguments as
[`epidist_newdata.epidist_marginal_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_marginal_model.md),
and fills the slots a summary row would use with the values an
individual row carries. The result is what
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)
and
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md)
need, so there is no need to copy a summary row out of the model data
and overwrite its covariates.

## Usage

``` r
# S3 method for class 'epidist_meta_model'
epidist_newdata(
  data,
  ...,
  pwindow = 0,
  swindow = 0,
  relative_obs_time = Inf,
  delay_min = 0
)
```

## Arguments

- data:

  An `epidist` data object, such as one returned by
  [`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.md),
  [`as_epidist_latent_model()`](https://epidist.epinowcast.org/reference/as_epidist_latent_model.md),
  [`as_epidist_naive_model()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.md)
  or
  [`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md).

- ...:

  Variables to expand into a grid, passed to
  [`tidyr::expand()`](https://tidyr.tidyverse.org/reference/expand.html).
  Supply the variables used in the model formula, such as `sex`. Each
  combination of their unique values becomes a row. Supply no variables
  to get a single row, which is what an intercept only model needs. A
  variable expanded here keeps its expanded values, so naming it as an
  argument of the method as well is an error.

- pwindow:

  Width of the primary event censoring window. Defaults to 0, which is
  no censoring.

- swindow:

  Width of the secondary event censoring window. Defaults to 0, which is
  no censoring.

- relative_obs_time:

  Observation time relative to the primary event. Defaults to `Inf`,
  which is no right truncation. `brms` warns about infinite values in
  the data when this is `Inf`. That warning is safe here, because
  prediction is done in R and never passes the value to Stan.

- delay_min:

  Minimum delay, the left truncation point. Defaults to 0, which is no
  left truncation.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
of `newdata` ready to predict from.

## Details

A model with a study level term, such as `mu ~ 1 + (1 | study)`, needs
either a `study` column in `newdata` or `re_formula = NA` when
predicting. Expand `study` here to predict for each study, or leave it
out and pass `re_formula = NA` for the population level delay. The
primary event distribution of the individual level rows is a parameter
of the model rather than a column, so it needs nothing here.

## See also

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
[`epidist_transform_data_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_meta_model.md),
[`is_epidist_meta_model()`](https://epidist.epinowcast.org/reference/is_epidist_meta_model.md),
[`new_epidist_meta_model()`](https://epidist.epinowcast.org/reference/new_epidist_meta_model.md)

Other newdata:
[`epidist_newdata()`](https://epidist.epinowcast.org/reference/epidist_newdata.md),
[`epidist_newdata.default()`](https://epidist.epinowcast.org/reference/epidist_newdata.default.md),
[`epidist_newdata.epidist_latent_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_latent_model.md),
[`epidist_newdata.epidist_marginal_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_marginal_model.md),
[`epidist_newdata.epidist_naive_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_naive_model.md)

## Examples

``` r
estimates <- as_epidist_estimates_data(
  data.frame(
    study = c("A", "A", "B"),
    type = c("mean", "sd", "mean"),
    value = c(7.5, 3.6, 6.4),
    n = c(120, 120, 80),
    relative_obs_time = c(20, 20, Inf),
    trunc_adjusted = c(FALSE, FALSE, TRUE),
    cens_adjusted = c(0, 0, 1)
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

# The population level delay, with no censoring and no truncation
epidist_newdata(meta)
#> # A tibble: 1 × 18
#>   delay_lwr obs_type study_n trunc_adjusted trunc_design cens_adjusted
#>       <dbl>    <int>   <int>          <int>        <int>         <int>
#> 1         0        1       0              0            0             0
#> # ℹ 12 more variables: group_start <int>, group_len <int>, chol_start <int>,
#> #   n_quad <int>, relative_obs_time <dbl>, pwindow <dbl>, swindow <dbl>,
#> #   delay_upr <dbl>, delay_min <dbl>, report_se <dbl>, quantile_p <dbl>,
#> #   growth_rate <dbl>

# A row for each study, with daily censoring
epidist_newdata(meta, study, pwindow = 1, swindow = 1)
#> # A tibble: 2 × 19
#>   study delay_lwr obs_type study_n trunc_adjusted trunc_design cens_adjusted
#>   <chr>     <dbl>    <int>   <int>          <int>        <int>         <int>
#> 1 A             0        1       0              0            0             0
#> 2 B             0        1       0              0            0             0
#> # ℹ 12 more variables: group_start <int>, group_len <int>, chol_start <int>,
#> #   n_quad <int>, relative_obs_time <dbl>, pwindow <dbl>, swindow <dbl>,
#> #   delay_upr <dbl>, delay_min <dbl>, report_se <dbl>, quantile_p <dbl>,
#> #   growth_rate <dbl>
```
