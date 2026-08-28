# Build `newdata` for the marginal model

The marginal model uses the response, the observation time relative to
the primary event, the primary and secondary censoring windows, the
upper bound of the delay and the minimum delay. This method adds all of
them. The response and its upper bound are set to 0. `brms` needs the
response in `newdata` and needs it to be a whole number for this model,
but prediction ignores its value. The rest are set here.

## Usage

``` r
# S3 method for class 'epidist_marginal_model'
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
  [`as_epidist_latent_model()`](https://epidist.epinowcast.org/reference/as_epidist_latent_model.md)
  or
  [`as_epidist_naive_model()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.md).

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

The defaults give the delay distribution with no censoring and no
truncation, which is a continuous probability density function. For a
discrete probability mass function with daily censoring set `pwindow`
and `swindow` to 1. For the delay distribution as it would be seen at a
given time set `relative_obs_time` to that time relative to the primary
event.

## See also

Other marginal_model:
[`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.md),
[`as_epidist_marginal_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.epidist_aggregate_data.md),
[`as_epidist_marginal_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.epidist_linelist_data.md),
[`epidist_family_model.epidist_marginal_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.epidist_marginal_model.md),
[`epidist_formula_model.epidist_marginal_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_marginal_model.md),
[`epidist_transform_data_model.epidist_marginal_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_marginal_model.md),
[`is_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/is_epidist_marginal_model.md),
[`new_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/new_epidist_marginal_model.md)

Other newdata:
[`epidist_newdata()`](https://epidist.epinowcast.org/reference/epidist_newdata.md),
[`epidist_newdata.default()`](https://epidist.epinowcast.org/reference/epidist_newdata.default.md),
[`epidist_newdata.epidist_latent_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_latent_model.md),
[`epidist_newdata.epidist_naive_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_naive_model.md)

## Examples

``` r
prep_obs <- sierra_leone_ebola_data |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ) |>
  as_epidist_marginal_model()
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).
#> ! Setting 8294 relative observation times (`relative_obs_time`) greater than 98
#>   (2x the maximum delay) to Inf.
#> ℹ This improves model efficiency by reducing the number of unique observation
#>   times in the data.
#> ℹ The impact on model accuracy should be negligible because these relative
#>   observation times are high enough to cause very limited right truncation.
#> ℹ The original relative observation times are available in
#>   `orig_relative_obs_time`.
#> ℹ Raise `obs_time_threshold` to avoid this behaviour.

# A row for each sex, with no censoring and no truncation
epidist_newdata(prep_obs, sex)
#> # A tibble: 3 × 7
#>   sex    delay_lwr relative_obs_time pwindow swindow delay_upr delay_min
#>   <chr>      <dbl>             <dbl>   <dbl>   <dbl>     <dbl>     <dbl>
#> 1 Female         0               Inf       0       0         0         0
#> 2 Male           0               Inf       0       0         0         0
#> 3 NA             0               Inf       0       0         0         0

# The same, with daily censoring
epidist_newdata(prep_obs, sex, pwindow = 1, swindow = 1)
#> # A tibble: 3 × 7
#>   sex    delay_lwr relative_obs_time pwindow swindow delay_upr delay_min
#>   <chr>      <dbl>             <dbl>   <dbl>   <dbl>     <dbl>     <dbl>
#> 1 Female         0               Inf       1       1         0         0
#> 2 Male           0               Inf       1       1         0         0
#> 3 NA             0               Inf       1       1         0         0
```
