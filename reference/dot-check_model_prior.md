# Model specific checks of user supplied prior distributions

Dispatches on the class of `data` so that a model can reject or warn
about prior distributions it does not support. By default no checks are
made.

## Usage

``` r
.check_model_prior(data, prior)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

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

## Value

`NULL`, invisibly, called for the messages it may raise.
