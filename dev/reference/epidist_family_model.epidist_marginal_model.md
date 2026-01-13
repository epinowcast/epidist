# Create the model-specific component of an `epidist` custom family

Create the model-specific component of an `epidist` custom family

## Usage

``` r
# S3 method for class 'epidist_marginal_model'
epidist_family_model(data, family, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- family:

  Output of a call to
  [`brms::brmsfamily()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  with additional information as provided by
  [`.add_dpar_info()`](https://epidist.epinowcast.org/dev/reference/dot-add_dpar_info.md)

- ...:

  Additional arguments passed to method.

## See also

Other marginal_model:
[`as_epidist_marginal_model()`](https://epidist.epinowcast.org/dev/reference/as_epidist_marginal_model.md),
[`as_epidist_marginal_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_marginal_model.epidist_aggregate_data.md),
[`as_epidist_marginal_model.epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_marginal_model.epidist_linelist_data.md),
[`epidist_formula_model.epidist_marginal_model()`](https://epidist.epinowcast.org/dev/reference/epidist_formula_model.epidist_marginal_model.md),
[`epidist_transform_data_model.epidist_marginal_model()`](https://epidist.epinowcast.org/dev/reference/epidist_transform_data_model.epidist_marginal_model.md),
[`is_epidist_marginal_model()`](https://epidist.epinowcast.org/dev/reference/is_epidist_marginal_model.md),
[`new_epidist_marginal_model()`](https://epidist.epinowcast.org/dev/reference/new_epidist_marginal_model.md)
