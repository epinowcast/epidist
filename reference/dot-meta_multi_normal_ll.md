# The log density of a study's summaries under a supplied covariance

A study that cannot share its delays can report a vector of summaries
with a covariance matrix over them, which keeps the correlation between
the quantities it reports. The Cholesky factor of that matrix is built
once when the model data are prepared and passed to Stan, so the sampler
never decomposes it.

## Usage

``` r
.meta_multi_normal_ll(y, implied, chol)
```

## Arguments

- y:

  A numeric vector of reported summaries.

- implied:

  A numeric vector of implied summaries from
  [`.meta_implied_summary_vector()`](https://epidist.epinowcast.org/reference/dot-meta_implied_summary_vector.md).

- chol:

  The lower triangular Cholesky factor of the reported covariance
  matrix.

## Value

A log density.
