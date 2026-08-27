# Check user supplied prior distributions for latent models

The latent model reparameterises the event windows so that `pwindow_raw`
and `swindow_raw` are on the unit interval. A non uniform prior on
`swindow_raw` is not supported and a non uniform prior on `pwindow_raw`
is only partially supported.

## Usage

``` r
# S3 method for class 'epidist_latent_model'
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
