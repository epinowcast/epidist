# Create a function to calculate the meta model log likelihood

Individual level rows use the marginal model log likelihood created by
[`epidist_gen_log_lik()`](https://epidist.epinowcast.org/reference/epidist_gen_log_lik.md).
Summary rows use the sampling distributions described in
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md),
evaluated at the implied summaries for each posterior draw. A row that
stands for several summaries reported by one study returns the joint log
likelihood of all of them, so an observation here is a group of
summaries rather than a single reported value.

## Usage

``` r
epidist_gen_meta_log_lik(family)
```

## Arguments

- family:

  A description of the response distribution and link function to be
  used in the model. Every family function has a link argument allowing
  users to specify the link function to be applied on the response
  variable. If not specified, default links are used. For details of all
  supported families see
  [`brms::brmsfamily()`](https://paulbuerkner.com/brms/reference/brmsfamily.html).
  Commonly used, such as
  [`brms::lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  are also reexported as part of `epidist`.

## Value

A function that calculates the log likelihood for a single observation.
The prep object must have the meta model `vint` and `vreal` slots.

## See also

[`brms::log_lik()`](https://paulbuerkner.com/brms/reference/log_lik.brmsfit.html)
for details on the brms log likelihood interface.

Other gen:
[`epidist_gen_log_lik()`](https://epidist.epinowcast.org/reference/epidist_gen_log_lik.md),
[`epidist_gen_meta_predict()`](https://epidist.epinowcast.org/reference/epidist_gen_meta_predict.md),
[`epidist_gen_posterior_epred()`](https://epidist.epinowcast.org/reference/epidist_gen_posterior_epred.md),
[`epidist_gen_posterior_predict()`](https://epidist.epinowcast.org/reference/epidist_gen_posterior_predict.md)
