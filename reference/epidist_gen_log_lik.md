# Create a function to calculate the marginalised log likelihood for double censored and truncated delay distributions

This function creates a log likelihood function that calculates the
marginal likelihood for a single observation by integrating over the
latent primary and secondary event windows. Where analytical solutions
exist in
[`primarycensored::dpcens()`](https://primarycensored.epinowcast.org/reference/dprimarycensored.html)
these are used, otherwise the integration is performed numerically.
[`primarycensored::dpcens()`](https://primarycensored.epinowcast.org/reference/dprimarycensored.html)
handles the double censoring and truncation of the delay distribution.

## Usage

``` r
epidist_gen_log_lik(family)
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

A function that calculates the marginal log likelihood for a single
observation. The prep object must have the following variables:

- `vreal1`: relative observation time

- `vreal2`: primary event window

- `vreal3`: secondary event window

## Details

The marginal likelihood accounts for uncertainty in both the primary and
secondary event windows by integrating over their possible values,
weighted by their respective uniform distributions.

## See also

[`brms::log_lik()`](https://paulbuerkner.com/brms/reference/log_lik.brmsfit.html)
for details on the brms log likelihood interface.

Other gen:
[`epidist_gen_posterior_epred()`](https://epidist.epinowcast.org/reference/epidist_gen_posterior_epred.md),
[`epidist_gen_posterior_predict()`](https://epidist.epinowcast.org/reference/epidist_gen_posterior_predict.md)
