# Create a function to draw from the meta model posterior predictive distribution

Individual level rows are predicted as in the marginal model using
[`epidist_gen_posterior_predict()`](https://epidist.epinowcast.org/reference/epidist_gen_posterior_predict.md).
For summary rows the predicted quantity is the reported summary itself,
that is a simulated reported mean, standard deviation, or, for quantile
rows, cumulative probability at the reported value. A row that stands
for several summaries reported by one study predicts the first of them,
drawn from its marginal. Predictions for summary rows are therefore not
on the delay scale and should not be compared directly with individual
level predictions. They come from the normal approximations described in
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md),
so a predicted cumulative probability for a quantile row can fall
outside \[0, 1\] when the study sample size is small.

## Usage

``` r
epidist_gen_meta_predict(family)
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

A function that takes a `prep` argument from brms and returns a matrix
of posterior predictions.

## See also

[`brms::posterior_predict()`](https://paulbuerkner.com/brms/reference/posterior_predict.brmsfit.html)
for details on how this is used within `brms`.

Other gen:
[`epidist_gen_log_lik()`](https://epidist.epinowcast.org/reference/epidist_gen_log_lik.md),
[`epidist_gen_meta_log_lik()`](https://epidist.epinowcast.org/reference/epidist_gen_meta_log_lik.md),
[`epidist_gen_posterior_epred()`](https://epidist.epinowcast.org/reference/epidist_gen_posterior_epred.md),
[`epidist_gen_posterior_predict()`](https://epidist.epinowcast.org/reference/epidist_gen_posterior_predict.md)
