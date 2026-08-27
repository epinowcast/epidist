# Combine the model and family specific prior distributions

Model specific priors from
[`epidist_model_prior()`](https://epidist.epinowcast.org/reference/epidist_model_prior.md)
overwrite family specific priors from
[`epidist_family_prior()`](https://epidist.epinowcast.org/reference/epidist_family_prior.md).
The result is then restricted to the parameters `brms` recognises, so
that priors for parameters which are not in the model are dropped.

## Usage

``` r
.internal_prior(data, family, formula, default)
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

- default:

  The default prior distributions from
  [`brms::default_prior()`](https://paulbuerkner.com/brms/reference/default_prior.html).

## Value

A `brmsprior` object, or `NULL` when there are no internal priors.
