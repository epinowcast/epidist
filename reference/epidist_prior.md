# Define custom prior distributions for epidist models

This function combines model specific prior distributions from
[`epidist_model_prior()`](https://epidist.epinowcast.org/reference/epidist_model_prior.md),
family specific prior distributions from
[`epidist_family_prior()`](https://epidist.epinowcast.org/reference/epidist_family_prior.md),
and user provided prior distributions into a single set of custom
priors. Each element overwrites previous elements, such that user
provided prior distributions have the highest priority. If a user prior
distribution is provided which is not a parameter of the model, a
warning will be shown.

## Usage

``` r
epidist_prior(
  data,
  family,
  formula,
  prior,
  merge = TRUE,
  enforce_presence = FALSE
)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- family:

  A description of the response distribution and link function to be
  used in the model created using
  [`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md).

- formula:

  A symbolic description of the model to be fitted created using
  [`epidist_formula()`](https://epidist.epinowcast.org/reference/epidist_formula.md).

- prior:

  One or more `brmsprior` objects created by
  [`brms::set_prior()`](https://paulbuerkner.com/brms/reference/set_prior.html)
  or related functions. These priors are passed to `epidist_prior()` in
  the `prior` argument. Some models have default priors that are
  automatically added (see
  [`epidist_model_prior()`](https://epidist.epinowcast.org/reference/epidist_model_prior.md)).
  These can be merged with user-provided priors using the `merge_priors`
  argument.

- merge:

  If `TRUE` then merge new priors with existing ones, if `FALSE` only
  use new priors. Defaults to `TRUE`. This may be useful if the built in
  approaches for merging priors are not flexible enough for a particular
  use case.

- enforce_presence:

  If `TRUE` then only allow user priors that match existing default
  priors. If `FALSE` then allow user priors that are not present in the
  default set. Defaults to `FALSE`.

## Value

A `brmsprior` object containing the combined custom prior distributions.

## Details

Note that the matching of priors is imperfect as it does not use brms'
internal prior matching functionality. For example, it cannot
distinguish between a prior for all coefficients (class = "b") and a
prior for a specific coefficient (class = "b" and coef specified).

Some models add parameters which `brms` does not know about, such as the
event windows of the latent model. Priors for these are written using
the `parameter ~ distribution` syntax of
[`brms::set_prior()`](https://paulbuerkner.com/brms/reference/set_prior.html)
and are passed to Stan unchanged. A prior written this way replaces any
existing prior for the same parameter and is not checked against the
parameters of the model. Note that the latent model requires a
`uniform(0, 1)` prior on its event windows.

## See also

Other prior:
[`epidist_family_prior()`](https://epidist.epinowcast.org/reference/epidist_family_prior.md),
[`epidist_family_prior.default()`](https://epidist.epinowcast.org/reference/epidist_family_prior.default.md),
[`epidist_family_prior.gengamma()`](https://epidist.epinowcast.org/reference/epidist_family_prior.gengamma.md),
[`epidist_family_prior.lognormal()`](https://epidist.epinowcast.org/reference/epidist_family_prior.lognormal.md),
[`epidist_family_prior.nonparametric()`](https://epidist.epinowcast.org/reference/epidist_family_prior.nonparametric.md),
[`epidist_model_prior()`](https://epidist.epinowcast.org/reference/epidist_model_prior.md),
[`epidist_model_prior.default()`](https://epidist.epinowcast.org/reference/epidist_model_prior.default.md),
[`epidist_model_prior.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_model_prior.epidist_meta_model.md)

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
epidist_prior(data, family = family, formula = formula, prior = NULL)
#> Warning: Found infinite values in the data, which may cause issues for Stan.
#>              prior     class coef group resp  dpar nlpar   lb   ub source  tag
#>       normal(1, 1) Intercept                             <NA> <NA> family <NA>
#>  normal(-0.7, 0.4) Intercept                 sigma       <NA> <NA> family <NA>
```
