# Convert an object to an `epidist_naive_model` object

Creates an `epidist_naive_model` object from various input formats. This
enables fitting naive models for epidemiological delays using
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md). The
naive model approach ignores censoring and truncation in the data, using
only the lower bounds of the intervals as point estimates. This is the
simplest approach but may lead to biased estimates if there is
substantial censoring or truncation in the data.

## Usage

``` r
as_epidist_naive_model(data, ...)
```

## Arguments

- data:

  An object to be converted to the class `epidist_naive_model`

- ...:

  Additional arguments passed to methods.

## See also

Other naive_model:
[`as_epidist_naive_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.epidist_aggregate_data.md),
[`as_epidist_naive_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.epidist_linelist_data.md),
[`epidist_formula_model.epidist_naive_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_naive_model.md),
[`epidist_transform_data_model.epidist_naive_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_naive_model.md),
[`is_epidist_naive_model()`](https://epidist.epinowcast.org/reference/is_epidist_naive_model.md),
[`new_epidist_naive_model()`](https://epidist.epinowcast.org/reference/new_epidist_naive_model.md)
