# Create the model-specific component of an `epidist` custom family

Create the model-specific component of an `epidist` custom family

## Usage

``` r
# S3 method for class 'epidist_meta_model'
epidist_family_model(data, family, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- family:

  Output of a call to
  [`brms::brmsfamily()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  with additional information as provided by
  [`.add_dpar_info()`](https://epidist.epinowcast.org/reference/dot-add_dpar_info.md)

- ...:

  Additional arguments passed to method.

## See also

Other meta_model:
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md),
[`as_epidist_meta_model.NULL()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.NULL.md),
[`as_epidist_meta_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_aggregate_data.md),
[`as_epidist_meta_model.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_estimates_data.md),
[`as_epidist_meta_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_linelist_data.md),
[`assert_epidist.epidist_meta_model()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_meta_model.md),
[`epidist_formula_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_meta_model.md),
[`epidist_model_prior.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_model_prior.epidist_meta_model.md),
[`epidist_newdata.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_meta_model.md),
[`epidist_transform_data_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_meta_model.md),
[`is_epidist_meta_model()`](https://epidist.epinowcast.org/reference/is_epidist_meta_model.md),
[`new_epidist_meta_model()`](https://epidist.epinowcast.org/reference/new_epidist_meta_model.md)
