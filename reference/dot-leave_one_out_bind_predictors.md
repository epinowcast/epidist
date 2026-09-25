# Attach the model's predictors from `newdata` to a comparison

`study` in `out` already names the held out study, so a `study`
predictor in `newdata`, such as one built with
`epidist_newdata(meta, study)`, is excluded to avoid a second column of
the same name.

## Usage

``` r
.leave_one_out_bind_predictors(out, newdata, formula)
```

## Arguments

- out:

  The comparison, as returned by
  [`.leave_one_out_compare()`](https://epidist.epinowcast.org/reference/dot-leave_one_out_compare.md),
  with `study` and `.row` columns.

- newdata:

  The `newdata` the comparison was built from.

- formula:

  The `brms` formula of the fit, used to find its predictors.

## Value

`out` with any predictors of `formula` present in `newdata`, other than
`study`, added after `.row`.
