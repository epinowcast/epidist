# The implied summaries of a joint study group and their sampling covariance

A continuous estimand reporting a mean or a standard deviation alongside
quantiles has every summary fitted jointly, with the covariance of
[`.meta_joint_covariance()`](https://epidist.epinowcast.org/reference/dot-meta_joint_covariance.md)
derived from the implied distribution rather than supplied. The
quantiles are read off the implied nodes as for a covariance matrix
group, see
[`.meta_implied_summary_vector()`](https://epidist.epinowcast.org/reference/dot-meta_implied_summary_vector.md),
and the density and centred partial moments at each are taken from the
same nodes.

## Usage

``` r
.meta_joint_study_terms(dist, args, slots, moments = NULL)
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

A list with the `implied` summary vector and the covariance matrix
`sigma`, or `NULL` where the implied moments are not finite, the nodes
underflow or a quantile sits where the estimand has no density, which
the caller rejects.
