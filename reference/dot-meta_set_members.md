# Attach the grouped summary members to an `epidist_meta_model` object

The members of a group row cannot be stored in the row itself because a
study may report any number of quantiles, so they are held alongside the
data and passed to Stan as flat arrays that the group row indexes into.
See
[`.meta_estimate_rows()`](https://epidist.epinowcast.org/reference/dot-meta_estimate_rows.md).

## Usage

``` r
.meta_set_members(data, members, chol = numeric(0))
```

## Arguments

- data:

  An `epidist_meta_model` object.

- members:

  A tibble of member `value`, `count`, `type` and `p` columns.

- chol:

  A flat numeric vector of Cholesky factor entries, in column major
  order, for the groups covered by a covariance matrix.

## Value

The input with the members attached.
