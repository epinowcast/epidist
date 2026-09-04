# Build a single meta model row from one group of summary estimates

The `group_start` slot is filled in by
[`.meta_estimate_rows()`](https://epidist.epinowcast.org/reference/dot-meta_estimate_rows.md)
once every group has been built.

## Usage

``` r
.meta_group_row(estimates, vcov = NULL, n_quad = .meta_n_quad())
```

## Arguments

- estimates:

  The rows of an `epidist_estimates_data` object making up one group.

- vcov:

  The covariance matrix over the group's summaries, or `NULL` where the
  study reported standard errors or a sample size instead.

- n_quad:

  The number of quadrature intervals the group's study is evaluated on,
  from
  [`.estimates_n_quad()`](https://epidist.epinowcast.org/reference/dot-estimates_n_quad.md).

## Value

A list with a one row tibble `row`, a tibble of its `members`, and the
flat `chol` entries of its covariance matrix.
