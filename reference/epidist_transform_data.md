# Transform data for an epidist model

This function is used within
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) to
transform data before passing to `brms`. It is unlikely that as a user
you will need this function, but we export it nonetheless to be
transparent about what happens inside of a call to
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

## Usage

``` r
epidist_transform_data(data, family, formula, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- family:

  A description of the response distribution and link function to be
  used in the model created using
  [`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md).

- formula:

  A formula object created using
  [`epidist_formula()`](https://epidist.epinowcast.org/reference/epidist_formula.md).

- ...:

  Additional arguments passed to `fn` method.

## See also

Other transform_data:
[`epidist_transform_data_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.md),
[`epidist_transform_data_model.default()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.default.md)
