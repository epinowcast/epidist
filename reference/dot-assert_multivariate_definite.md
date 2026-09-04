# Check that a covariance matrix has a usable Cholesky factor

A set of quantities that are deterministic functions of fewer underlying
parameters has a covariance of the rank of those parameters, not of its
own dimension. Five summaries of a two parameter fit are the common
case. The message says so, because the alternative reading, that a
column is constant or repeated, is usually not what happened. The rank
is judged on the correlation matrix against
[`.multivariate_eigen_floor()`](https://epidist.epinowcast.org/reference/dot-multivariate_eigen_floor.md),
because a covariance that is singular only up to the curvature of the
map from parameters to summaries still has a Cholesky factor.

## Usage

``` r
.assert_multivariate_definite(vcov)
```

## Arguments

- vcov:

  A covariance matrix.

## Value

`NULL`, invisibly.
