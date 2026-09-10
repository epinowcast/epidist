# The joint log likelihood of the summaries of one continuous study

The multivariate normal of
[`.meta_joint_study_terms()`](https://epidist.epinowcast.org/reference/dot-meta_joint_study_terms.md).
A covariance that is not positive definite, which quadrature error can
produce for a draw far from the reported summaries, is rejected with a
log likelihood of `-Inf`, as are the failures listed there. Matches
`meta_family_joint_study_lpdf()` in Stan.

## Usage

``` r
.meta_joint_study_ll(y, dist, args, slots, moments = NULL)
```

## Arguments

- y:

  A numeric vector of reported summaries in member order.

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

A log density.
