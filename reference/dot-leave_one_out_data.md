# The model data used to hold each study out of a meta model fit

The model data used to hold each study out of a meta model fit

## Usage

``` r
.leave_one_out_data(fit, data = NULL)
```

## Arguments

- fit:

  A meta model fitted with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) to
  an `epidist_meta_model` object.

- data:

  The `epidist_meta_model` object `fit` was fitted to. Only needed when
  `fit$data` has no `study` column, which happens when the formula does
  not use `study`. If `NULL`, the default, the model data stored in
  `fit` is used.

## Value

The `epidist_meta_model` object given as `data`, or the plain
`data.frame` of model data stored in `fit`.
