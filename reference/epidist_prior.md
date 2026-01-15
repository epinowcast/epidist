# Define custom prior distributions for epidist models

This function combines model specific prior distributions from
[`epidist_model_prior()`](https://epidist.epinowcast.org/reference/epidist_model_prior.md),
family specific prior distributions from
[`epidist_family_prior()`](https://epidist.epinowcast.org/reference/epidist_family_prior.md),
and user provided prior distributions into a single set of custom
priors. Each element overwrites previous elements, such that user
provided prior distributions have the highest priority. If a user prior
distribution is provided which is not included in the model, a warning
will be shown.

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

## See also

Other prior:
[`epidist_family_prior()`](https://epidist.epinowcast.org/reference/epidist_family_prior.md),
[`epidist_family_prior.default()`](https://epidist.epinowcast.org/reference/epidist_family_prior.default.md),
[`epidist_family_prior.lognormal()`](https://epidist.epinowcast.org/reference/epidist_family_prior.lognormal.md),
[`epidist_model_prior()`](https://epidist.epinowcast.org/reference/epidist_model_prior.md),
[`epidist_model_prior.default()`](https://epidist.epinowcast.org/reference/epidist_model_prior.default.md)
