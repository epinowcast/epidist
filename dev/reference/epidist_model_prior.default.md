# Default model specific prior distributions

By default, we do not return any model specific prior distributions.

## Usage

``` r
# Default S3 method
epidist_model_prior(data, formula, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- formula:

  An object of class
  [stats::formula](https://rdrr.io/r/stats/formula.html) or
  [brms::brmsformula](https://paulbuerkner.com/brms/reference/brmsformula.html)
  (or one that can be coerced to those classes). A symbolic description
  of the model to be fitted. A formula must be provided for the
  distributional parameter `mu`, and may optionally be provided for
  other distributional parameters.

- ...:

  Additional arguments passed to `fn` method.

## See also

Other prior:
[`epidist_family_prior()`](https://epidist.epinowcast.org/dev/reference/epidist_family_prior.md),
[`epidist_family_prior.default()`](https://epidist.epinowcast.org/dev/reference/epidist_family_prior.default.md),
[`epidist_family_prior.lognormal()`](https://epidist.epinowcast.org/dev/reference/epidist_family_prior.lognormal.md),
[`epidist_model_prior()`](https://epidist.epinowcast.org/dev/reference/epidist_model_prior.md),
[`epidist_prior()`](https://epidist.epinowcast.org/dev/reference/epidist_prior.md)
