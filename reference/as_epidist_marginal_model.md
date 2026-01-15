# Convert an object to an `epidist_marginal_model` object

Creates an `epidist_marginal_model` object from various input formats.
This enables fitting marginal models for epidemiological delays using
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md). The
marginal model approach uses the likelihood from the
[primarycensored](https://primarycensored.epinowcast.org/) package to
efficiently handle censoring in both primary and secondary events as
well as truncation due to observation times. See the specific methods
[`as_epidist_marginal_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.epidist_linelist_data.md)
and
[`as_epidist_marginal_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.epidist_aggregate_data.md)
for details on supported input formats and usage examples.

## Usage

``` r
as_epidist_marginal_model(data, ...)
```

## Arguments

- data:

  An object to be converted to the class `epidist_marginal_model`

- ...:

  Additional arguments passed to methods.

## See also

Other marginal_model:
[`as_epidist_marginal_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.epidist_aggregate_data.md),
[`as_epidist_marginal_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.epidist_linelist_data.md),
[`epidist_family_model.epidist_marginal_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.epidist_marginal_model.md),
[`epidist_formula_model.epidist_marginal_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_marginal_model.md),
[`epidist_transform_data_model.epidist_marginal_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_marginal_model.md),
[`is_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/is_epidist_marginal_model.md),
[`new_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/new_epidist_marginal_model.md)
