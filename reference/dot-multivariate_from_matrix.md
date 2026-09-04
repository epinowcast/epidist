# Build an `epidist_multivariate` object from a matrix of draws

Build an `epidist_multivariate` object from a matrix of draws

## Usage

``` r
.multivariate_from_matrix(wide, params, index)
```

## Arguments

- wide:

  A numeric matrix with one row per draw and one column per element.

- params:

  The parameter names, in order.

- index:

  The trajectory points, in order.

## Value

An object of class `epidist_multivariate`.
