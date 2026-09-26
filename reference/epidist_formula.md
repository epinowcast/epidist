# Define a model specific formula

This function is used within
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) to
create the formula object passed to `brms`. It is unlikely that as a
user you will need this function, but we export it nonetheless to be
transparent about what exactly is happening inside of a call to
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

## Usage

``` r
epidist_formula(data, family, formula, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- family:

  A description of the response distribution and link function to be
  used in the model created using
  [`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md).

- formula:

  An object of class
  [stats::formula](https://rdrr.io/r/stats/formula.html) or
  [brms::brmsformula](https://paulbuerkner.com/brms/reference/brmsformula.html)
  (or one that can be coerced to those classes). A symbolic description
  of the model to be fitted. A formula must be provided for the
  distributional parameter `mu`, and may optionally be provided for
  other distributional parameters.

- ...:

  Additional arguments passed to `fn` method.

## Value

A `brmsformula` object.

## See also

Other formula:
[`epidist_formula_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.md),
[`epidist_formula_model.default()`](https://epidist.epinowcast.org/reference/epidist_formula_model.default.md)

## Examples

``` r
data <- sierra_leone_ebola_data |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ) |>
  as_epidist_aggregate_data() |>
  as_epidist_marginal_model()
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
family <- epidist_family(data, family = lognormal())
formula <- epidist_formula(data, family = family, formula = mu ~ 1)
#> Warning: Found infinite values in the data, which may cause issues for Stan.
formula
#> delay_lwr | weights(n) + vreal(relative_obs_time, pwindow, swindow, delay_upr, delay_min) ~ 1 
#> sigma ~ 1
```
