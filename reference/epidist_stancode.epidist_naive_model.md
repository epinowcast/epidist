# Define the Stan code of the naive model

The naive model uses the `brms` family as it is, so it only needs Stan
code for a family `brms` does not have, such as
[`gengamma()`](https://epidist.epinowcast.org/reference/gengamma.md),
whose density `brms` calls by name.

## Usage

``` r
# S3 method for class 'epidist_naive_model'
epidist_stancode(
  data,
  family = epidist_family(data),
  formula = epidist_formula(data),
  ...
)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- family:

  The epidist family object specifying the distribution

- formula:

  The model formula

- ...:

  Additional arguments passed to `fn` method.

## Value

A list of `stanvars` objects, or `NULL` when none are needed.

## See also

Other naive_model:
[`as_epidist_naive_model()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.md),
[`as_epidist_naive_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.epidist_aggregate_data.md),
[`as_epidist_naive_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.epidist_linelist_data.md),
[`epidist_formula_model.epidist_naive_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_naive_model.md),
[`epidist_newdata.epidist_naive_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_naive_model.md),
[`epidist_transform_data_model.epidist_naive_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_naive_model.md),
[`is_epidist_naive_model()`](https://epidist.epinowcast.org/reference/is_epidist_naive_model.md),
[`new_epidist_naive_model()`](https://epidist.epinowcast.org/reference/new_epidist_naive_model.md)
