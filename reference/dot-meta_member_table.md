# Build the member table of one joint likelihood group

The member type and probability are carried alongside the reported value
because a group covered by a covariance matrix may mix means, standard
deviations and quantiles, so the likelihood needs to know which implied
summary each member is.

## Usage

``` r
.meta_member_table(estimates, count, lower = 0L)
```

## Arguments

- estimates:

  The rows of an `epidist_estimates_data` object making up one group,
  already ordered.

- count:

  The cumulative counts the multinomial quantile likelihood uses, the
  largest counts below each reported day for quantiles of integer day
  delays, or zeros for a group that uses neither.

- lower:

  The smallest counts at or below each reported day for quantiles of
  integer day delays, or zeros otherwise.

## Value

A tibble of member `value`, `count`, `lower`, `type` and `p` columns.

## Details

A set of quantiles of integer day delays carries the box each reported
day puts on the study's cumulative counts: `count` is the largest number
of delays the study can have seen below the day and `lower` the smallest
at or below it. See
[`.meta_grid_box_ll()`](https://epidist.epinowcast.org/reference/dot-meta_grid_box_ll.md).
