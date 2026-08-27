# Check for a non uniform prior on a latent model event window

Check for a non uniform prior on a latent model event window

## Usage

``` r
.has_non_uniform_window_prior(prior, parameter)
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

- parameter:

  The name of the event window parameter to check.

## Value

A logical, `TRUE` if a non uniform prior has been supplied.
