# Validate model data

Replaces the checks that `epidist` relies on from
`brms:::validate_data()`. `epidist` calls the `brms` internal only for
its errors and discards the returned model frame, so this helper checks
the same conditions and returns `data` invisibly. It errors when `data`
cannot be coerced to a `data.frame`, when it has no rows, when a
variable used in the formula is absent, when a column name contains a
double underscore or ends in an underscore, or when no complete case
remains. It warns when a used column contains infinite values.

## Usage

``` r
.validate_data(data, bterms)
```

## Arguments

- data:

  A `data.frame` containing the model data.

- bterms:

  An object returned by
  [`brms::brmsterms()`](https://paulbuerkner.com/brms/reference/brmsterms.html).
