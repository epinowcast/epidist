# Define the model-specific component of an `epidist` custom formula for the naive model

Define the model-specific component of an `epidist` custom formula for
the naive model

## Usage

``` r
# S3 method for class 'epidist_naive_model'
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

Other naive_model:
[`as_epidist_naive_model()`](https://epidist.epinowcast.org/dev/reference/as_epidist_naive_model.md),
[`as_epidist_naive_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_naive_model.epidist_aggregate_data.md),
[`as_epidist_naive_model.epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_naive_model.epidist_linelist_data.md),
[`epidist_transform_data_model.epidist_naive_model()`](https://epidist.epinowcast.org/dev/reference/epidist_transform_data_model.epidist_naive_model.md),
[`is_epidist_naive_model()`](https://epidist.epinowcast.org/dev/reference/is_epidist_naive_model.md),
[`new_epidist_naive_model()`](https://epidist.epinowcast.org/dev/reference/new_epidist_naive_model.md)
