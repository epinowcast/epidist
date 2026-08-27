# Warn about user priors which are not parameters of the model

Manually specified priors are passed to Stan unchanged and so are not
checked here.

## Usage

``` r
.warn_unmatched_prior(prior, known)
```

## Arguments

- prior:

  One or more `brmsprior` objects created by
  [`brms::set_prior()`](https://paulbuerkner.com/brms/reference/set_prior.html)
  or related functions. These priors are passed to
  [`epidist_prior()`](https://epidist.epinowcast.org/reference/epidist_prior.md)
  in the `prior` argument. Some models have default priors that are
  automatically added (see
  [`epidist_model_prior()`](https://epidist.epinowcast.org/reference/epidist_model_prior.md)).
  These can be merged with user-provided priors using the `merge_priors`
  argument.

- known:

  One or more prior distributions in the class `brmsprior` covering the
  parameters of the model.

## Value

`NULL`, invisibly, called for the warning it may raise.
