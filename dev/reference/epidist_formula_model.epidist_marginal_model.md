# Define the model-specific component of an `epidist` custom formula for the marginal model

Define the model-specific component of an `epidist` custom formula for
the marginal model

## Usage

``` r
# S3 method for class 'epidist_marginal_model'
epidist_formula_model(data, formula, ...)
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

  Additional arguments passed to method.

## See also

Other marginal_model:
[`as_epidist_marginal_model()`](https://epidist.epinowcast.org/dev/reference/as_epidist_marginal_model.md),
[`as_epidist_marginal_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_marginal_model.epidist_aggregate_data.md),
[`as_epidist_marginal_model.epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_marginal_model.epidist_linelist_data.md),
[`epidist_family_model.epidist_marginal_model()`](https://epidist.epinowcast.org/dev/reference/epidist_family_model.epidist_marginal_model.md),
[`epidist_transform_data_model.epidist_marginal_model()`](https://epidist.epinowcast.org/dev/reference/epidist_transform_data_model.epidist_marginal_model.md),
[`is_epidist_marginal_model()`](https://epidist.epinowcast.org/dev/reference/is_epidist_marginal_model.md),
[`new_epidist_marginal_model()`](https://epidist.epinowcast.org/dev/reference/new_epidist_marginal_model.md)
