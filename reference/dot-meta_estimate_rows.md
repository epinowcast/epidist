# Build the summary estimate rows of an `epidist_meta_model` object

One row is built per group from
[`.meta_assign_groups()`](https://epidist.epinowcast.org/reference/dot-meta_assign_groups.md).
A group of one is a single reported summary, and a larger group is
fitted with the joint likelihood of its members. The members of a group
are held in flat arrays that the row indexes with `group_start` and
`group_len`, because a study may report any number of quantiles and a
row has a fixed number of slots. Row order is irrelevant to the index,
so aggregating or reordering rows later cannot break it.

## Usage

``` r
.meta_estimate_rows(estimates)
```

## Arguments

- estimates:

  An `epidist_estimates_data` object.

## Value

A list with a tibble of summary `rows` using the meta model slots, a
tibble of their `members`, and the flat `chol` vector of Cholesky
factors for the groups covered by a covariance matrix.
