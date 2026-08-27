# Describe prior distributions for use in messages

Gives each prior as its distribution followed by the parameter it
applies to, dropping the matching columns which are empty. Braces are
escaped so that the result can be passed to `cli`.

## Usage

``` r
.describe_prior(prior)
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

## Value

A character vector with one entry per prior.
