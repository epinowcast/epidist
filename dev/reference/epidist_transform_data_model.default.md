# Default method for transforming data for a model

Default method for transforming data for a model

## Usage

``` r
# Default S3 method
epidist_transform_data_model(data, family, formula, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- family:

  A description of the response distribution and link function to be
  used in the model created using
  [`epidist_family()`](https://epidist.epinowcast.org/dev/reference/epidist_family.md).

- formula:

  A formula object created using
  [`epidist_formula()`](https://epidist.epinowcast.org/dev/reference/epidist_formula.md).

- ...:

  Additional arguments passed to `fn` method.

## See also

Other transform_data:
[`epidist_transform_data()`](https://epidist.epinowcast.org/dev/reference/epidist_transform_data.md),
[`epidist_transform_data_model()`](https://epidist.epinowcast.org/dev/reference/epidist_transform_data_model.md)
