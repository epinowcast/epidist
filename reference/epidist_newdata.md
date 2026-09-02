# Build `newdata` for prediction from an `epidist` data object

Prediction with `brms` and `tidybayes` needs a `newdata` argument
holding every variable the model uses. As well as the variables in your
formula that means the response and the observation process variables
that `epidist` adds. Their names depend on the model, so building
`newdata` by hand means knowing how each model is specified. This
function builds it for you. It expands the variables you supply into a
grid, in the same way as
[`tidyr::expand()`](https://tidyr.tidyverse.org/reference/expand.html),
and adds the rest with values you set through named arguments.

## Usage

``` r
epidist_newdata(data, ...)
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

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
of `newdata` ready to predict from.

## Details

The defaults give the delay distribution with no censoring and no
truncation. Set `pwindow` and `swindow` to ask for censoring, and
`relative_obs_time` to ask for truncation. See the method for each model
for the arguments it takes.

The result is a plain
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).
Pass it to
[`brms::posterior_epred()`](https://paulbuerkner.com/brms/reference/posterior_epred.brmsfit.html),
[`brms::posterior_predict()`](https://paulbuerkner.com/brms/reference/posterior_predict.brmsfit.html),
[`add_delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)
or
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md),
or to the `tidybayes` functions `add_epred_draws()` and
`add_predicted_draws()`.

## See also

Other newdata:
[`epidist_newdata.default()`](https://epidist.epinowcast.org/reference/epidist_newdata.default.md),
[`epidist_newdata.epidist_latent_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_latent_model.md),
[`epidist_newdata.epidist_marginal_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_marginal_model.md),
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

# An intercept only model
epidist_newdata(prep_obs)
#> # A tibble: 1 × 6
#>   delay_lwr relative_obs_time pwindow swindow delay_upr delay_min
#>       <dbl>             <dbl>   <dbl>   <dbl>     <dbl>     <dbl>
#> 1         0               Inf       0       0         0         0

# A row for each sex
epidist_newdata(prep_obs, sex)
#> # A tibble: 3 × 7
#>   sex    delay_lwr relative_obs_time pwindow swindow delay_upr delay_min
#>   <chr>      <dbl>             <dbl>   <dbl>   <dbl>     <dbl>     <dbl>
#> 1 Female         0               Inf       0       0         0         0
#> 2 Male           0               Inf       0       0         0         0
#> 3 NA             0               Inf       0       0         0         0
```
