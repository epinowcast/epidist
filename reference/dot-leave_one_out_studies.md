# The studies of a meta model to hold out

The studies of a meta model to hold out

## Usage

``` r
.leave_one_out_studies(data, studies = NULL)
```

## Arguments

- data:

  An `epidist_meta_model` object or the model data of a meta model fit,
  with a `study` column.

- studies:

  A character vector of the studies to hold out, one refit each. If
  `NULL`, the default, every study is held out in turn. Holding out a
  few studies, such as the largest, keeps the cost down when a model has
  many.

## Value

A character vector of study labels, in order of first appearance, or
`studies` in the order given.
