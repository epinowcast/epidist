# Check the covariance matrices of an `epidist_estimates_data` object

Each matrix must cover the rows sharing its `mvn_id`, be symmetric, and
be positive definite, so that it has a Cholesky factor and defines a
proper multivariate normal.

## Usage

``` r
.assert_estimates_vcov(data)
```

## Arguments

- data:

  An `epidist_estimates_data` object.

## Value

`NULL`, invisibly.
