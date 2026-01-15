# Create a function to draw from the posterior predictive distribution for a double censored and truncated delay distribution

This function creates a function that draws from the posterior
predictive distribution for a latent model using
[`primarycensored::rpcens()`](https://primarycensored.epinowcast.org/reference/rprimarycensored.html)
to handle censoring and truncation. The returned function takes a `prep`
argument from `brms` and returns posterior predictions. This is used
internally by
[`brms::posterior_predict()`](https://paulbuerkner.com/brms/reference/posterior_predict.brmsfit.html)
to generate predictions for latent models.

## Usage

``` r
epidist_gen_posterior_predict(family)
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

A function that takes a `prep` argument from brms and returns a matrix
of posterior predictions, with one row per posterior draw and one column
per observation. The `prep` object must have the following variables:

- `vreal1`: relative observation time

- `vreal2`: primary event window

- `vreal3`: secondary event window

## See also

[`brms::posterior_predict()`](https://paulbuerkner.com/brms/reference/posterior_predict.brmsfit.html)
for details on how this is used within `brms`,
[`primarycensored::rpcens()`](https://primarycensored.epinowcast.org/reference/rprimarycensored.html)
for details on the censoring approach

Other gen:
[`epidist_gen_log_lik()`](https://epidist.epinowcast.org/reference/epidist_gen_log_lik.md),
[`epidist_gen_posterior_epred()`](https://epidist.epinowcast.org/reference/epidist_gen_posterior_epred.md)
