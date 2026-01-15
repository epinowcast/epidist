# Extract samples of the delay distribution parameters

Extract samples of the delay distribution parameters

## Usage

``` r
predict_delay_parameters(fit, newdata = NULL, ...)

predict_dpar(fit, newdata = NULL, ...)
```

## Arguments

- fit:

  A model fit with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

- newdata:

  An optional data.frame for which to evaluate predictions. If `NULL`
  (default), the original data of the model is used. `NA` values within
  factors (excluding grouping variables) are interpreted as if all dummy
  variables of this factor are zero. This allows, for instance, to make
  predictions of the grand mean when using sum coding. `NA` values
  within grouping variables are treated as a new level.

- ...:

  Additional arguments passed to
  [`brms::prepare_predictions()`](https://paulbuerkner.com/brms/reference/prepare_predictions.html).

## See also

Other postprocess:
[`add_mean_sd()`](https://epidist.epinowcast.org/reference/add_mean_sd.md),
[`add_mean_sd.default()`](https://epidist.epinowcast.org/reference/add_mean_sd.default.md),
[`add_mean_sd.gamma_samples()`](https://epidist.epinowcast.org/reference/add_mean_sd.gamma_samples.md),
[`add_mean_sd.lognormal_samples()`](https://epidist.epinowcast.org/reference/add_mean_sd.lognormal_samples.md),
[`add_mean_sd.weibull_samples()`](https://epidist.epinowcast.org/reference/add_mean_sd.weibull_samples.md)
