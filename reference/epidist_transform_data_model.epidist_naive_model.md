# Transform data for the naive model

This method transforms data into the format required by the naive model
by:

1.  Identifying required columns for the naive model

2.  Summarising the data by counting unique combinations of these
    columns and any variables in the model formula using
    [`.summarise_n_by_formula()`](https://epidist.epinowcast.org/reference/dot-summarise_n_by_formula.md)

3.  Converting the summarised data to a naive model object using
    [`new_epidist_naive_model()`](https://epidist.epinowcast.org/reference/new_epidist_naive_model.md)

4.  Informing the user about any data aggregation that occurred using
    [`.inform_data_summarised()`](https://epidist.epinowcast.org/reference/dot-inform_data_summarised.md)

## Usage

``` r
# S3 method for class 'epidist_naive_model'
epidist_transform_data_model(data, family, formula, ...)
```

## Arguments

- data:

  The data to transform

- family:

  The epidist family object specifying the distribution

- formula:

  The model formula

- ...:

  Additional arguments passed to methods

## See also

Other naive_model:
[`as_epidist_naive_model()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.md),
[`as_epidist_naive_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.epidist_aggregate_data.md),
[`as_epidist_naive_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.epidist_linelist_data.md),
[`epidist_formula_model.epidist_naive_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_naive_model.md),
[`is_epidist_naive_model()`](https://epidist.epinowcast.org/reference/is_epidist_naive_model.md),
[`new_epidist_naive_model()`](https://epidist.epinowcast.org/reference/new_epidist_naive_model.md)
