# Model specific prior distributions for the meta model

The response column of a meta model is a placeholder on every summary
row, and `brms` centres its default prior for the intercept of `mu` on
the response. For a model fitted to summaries alone that default is
centred on a delay of zero. This method puts a `normal(1, 1)` prior on
the intercept instead, the scale of the lognormal family prior in
[`epidist_family_prior()`](https://epidist.epinowcast.org/reference/epidist_family_prior.md),
so that a Gamma or Weibull meta fit gets a prior on the same scale as a
lognormal one. On the log scale it is a median delay of about 3 days
with a 95% range of roughly 0.4 to 20 days.

## Usage

``` r
# S3 method for class 'epidist_meta_model'
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

## Value

A `brmsprior` object, or `NULL` when the model adds no priors.

## Details

The centre is fixed rather than taken from the reported values, because
a prior chosen from the data is not a prior. It would put the posterior
of a small review where the data already sit and understate how much the
studies disagree. The prior is added where `mu` is on the log scale,
which is the lognormal family, whose `mu` is the log of the median under
an identity link, and any family with a log link. Nothing is added for
other links, and a model with individual level rows only adds no prior,
so the family or `brms` default applies as it does for the marginal
model.

The between study spread of any group level term, such as `(1 | study)`,
gets a half normal prior with a standard deviation of 0.25 on the scale
of the linear predictor, so that a small review cannot fit that spread
from almost nothing under the wide `brms` default. It is dropped where
the formula has no group level term. The prior on the intercept of the
other distributional parameters is left to the family or to `brms`.

## See also

Other prior:
[`epidist_family_prior()`](https://epidist.epinowcast.org/reference/epidist_family_prior.md),
[`epidist_family_prior.default()`](https://epidist.epinowcast.org/reference/epidist_family_prior.default.md),
[`epidist_family_prior.lognormal()`](https://epidist.epinowcast.org/reference/epidist_family_prior.lognormal.md),
[`epidist_model_prior()`](https://epidist.epinowcast.org/reference/epidist_model_prior.md),
[`epidist_model_prior.default()`](https://epidist.epinowcast.org/reference/epidist_model_prior.default.md),
[`epidist_prior()`](https://epidist.epinowcast.org/reference/epidist_prior.md)

Other meta_model:
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md),
[`as_epidist_meta_model.NULL()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.NULL.md),
[`as_epidist_meta_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_aggregate_data.md),
[`as_epidist_meta_model.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_estimates_data.md),
[`as_epidist_meta_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_linelist_data.md),
[`assert_epidist.epidist_meta_model()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_meta_model.md),
[`epidist_family_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.epidist_meta_model.md),
[`epidist_formula_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_meta_model.md),
[`epidist_newdata.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_meta_model.md),
[`epidist_transform_data_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_meta_model.md),
[`is_epidist_meta_model()`](https://epidist.epinowcast.org/reference/is_epidist_meta_model.md),
[`new_epidist_meta_model()`](https://epidist.epinowcast.org/reference/new_epidist_meta_model.md)
