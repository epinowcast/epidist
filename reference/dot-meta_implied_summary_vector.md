# The summaries a study would report, one per multivariate normal member

Member types are 1 for a mean, 2 for a standard deviation and 3 for a
quantile at the matching probability. Quantile members are read off the
implied distribution function by
[`.meta_node_quantile()`](https://epidist.epinowcast.org/reference/dot-meta_node_quantile.md),
so they are on the delay scale, matching the reported values and the
covariance matrix supplied with them.

## Usage

``` r
.meta_implied_summary_vector(dist, args, slots, moments = NULL)
```

## Arguments

- dist:

  A `primarycensored` distribution function name.

- args:

  A named list of distribution parameters.

- slots:

  The output of
  [`.meta_row_slots()`](https://epidist.epinowcast.org/reference/dot-meta_row_slots.md).

- moments:

  A summary vector from
  [`.meta_implied_moments()`](https://epidist.epinowcast.org/reference/dot-meta_implied_moments.md)
  for this row and draw, or `NULL` to compute it here.

## Value

A numeric vector of implied summaries.
