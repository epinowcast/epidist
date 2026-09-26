# Define `epidist` family

This function is used within
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) to
create a model specific custom `brms` family object. This custom family
is passed to `brms`. It is unlikely that as a user you will need this
function, but we export it nonetheless to be transparent about what
happens inside of a call to
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

## Usage

``` r
epidist_family(data, family = lognormal(), ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- family:

  A description of the response distribution and link function to be
  used in the model. Every family function has a link argument allowing
  users to specify the link function to be applied on the response
  variable. If not specified, default links are used. For details of all
  supported families see
  [`brms::brmsfamily()`](https://paulbuerkner.com/brms/reference/brmsfamily.html).
  Commonly used, such as
  [`brms::lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  are also reexported as part of `epidist`.

- ...:

  Additional arguments passed to `fn` method.

## Value

A `brms` custom family object.

## Details

The family may be any `brms` family of a positive response, such as
[`brms::lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
`Gamma(link = "log")` or
[`brms::weibull()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
or a family `epidist` defines itself, such as
[`gengamma()`](https://epidist.epinowcast.org/reference/gengamma.md).

## See also

Other family:
[`epidist_family_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.md),
[`epidist_family_model.default()`](https://epidist.epinowcast.org/reference/epidist_family_model.default.md),
[`epidist_family_param()`](https://epidist.epinowcast.org/reference/epidist_family_param.md),
[`epidist_family_param.default()`](https://epidist.epinowcast.org/reference/epidist_family_param.default.md),
[`epidist_family_param.gengamma()`](https://epidist.epinowcast.org/reference/epidist_family_param.gengamma.md),
[`epidist_family_param.nonparametric()`](https://epidist.epinowcast.org/reference/epidist_family_param.nonparametric.md),
[`gengamma()`](https://epidist.epinowcast.org/reference/gengamma.md),
[`nonparametric()`](https://epidist.epinowcast.org/reference/nonparametric.md)

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
family
#> 
#> Custom family: marginal_lognormal 
#> Link function: identity 
#> Parameters: mu, sigma 
#> 
```
