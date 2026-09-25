# Remove one study from the model data of a meta model

An `epidist_meta_model` object is subset through the
[epidist_data](https://epidist.epinowcast.org/reference/epidist_data.md)
methods, which check the result, and then transformed with
[`epidist_transform_data()`](https://epidist.epinowcast.org/reference/epidist_transform_data.md)
so that its rows are summarised as they were for the full fit. The plain
model data stored in a fit has already been transformed, so it is subset
directly.

## Usage

``` r
.drop_study(data, study, fit)
```

## Arguments

- data:

  An `epidist_meta_model` object or the model data of a meta model fit,
  with a `study` column.

- study:

  The label of the study to remove.

- fit:

  The fit whose family and formula the transform uses.

## Value

The model data without the rows of `study`.
