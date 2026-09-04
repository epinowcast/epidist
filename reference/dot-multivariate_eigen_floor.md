# The smallest relative eigenvalue a reported covariance may have

Applied to the correlation matrix, so that it does not depend on the
scales of the reported quantities. A covariance over summaries that are
functions of fewer parameters is singular up to the curvature of the map
and Monte Carlo error, which leaves an eigenvalue around a millionth of
the largest rather than zero. A likelihood built on such a matrix
charges any error in the implied summaries, of numerical or family
origin, against that eigenvalue, so it is refused.

## Usage

``` r
.multivariate_eigen_floor()
```

## Value

A number.
