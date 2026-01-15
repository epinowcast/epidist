# Create a function to draw from the expected value of the posterior predictive distribution for a model

This function creates a function that calculates the expected value of
the posterior predictive distribution for a latent model. The returned
function takes a `prep` argument (from brms) and returns posterior
expected values. This is used internally by
[`brms::posterior_epred()`](https://paulbuerkner.com/brms/reference/posterior_epred.brmsfit.html)
to calculate expected values for latent models.

## Usage

``` r
epidist_gen_posterior_epred(family)
```

## Arguments

- family:

  A description of the response distribution and link function to be
  used in the model. Every family function has a link argument allowing
  users to specify the link function to be applied on the response
  variable. If not specified, default links are used. For details of all
  supported families see `brmsfamily()`. Commonly used, such as
  [`lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  are also reexported as part of `epidist`.

## Value

A function that takes a prep argument from brms and returns a matrix of
posterior expected values, with one row per posterior draw and one
column per observation.

## See also

[`brms::posterior_epred()`](https://paulbuerkner.com/brms/reference/posterior_epred.brmsfit.html)
for details on how this is used within `brms`.

Other gen:
[`epidist_gen_log_lik()`](https://epidist.epinowcast.org/reference/epidist_gen_log_lik.md),
[`epidist_gen_posterior_predict()`](https://epidist.epinowcast.org/reference/epidist_gen_posterior_predict.md)
