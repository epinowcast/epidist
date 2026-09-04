# The flat Cholesky factors of an `epidist_meta_model` object

A group covered by a covariance matrix has the Cholesky factor of that
matrix stored here, in column major order, and its row indexes into this
vector with `chol_start`. Factoring once here rather than inside the
likelihood keeps the matrix out of every gradient evaluation.

## Usage

``` r
.meta_chol(data)
```

## Arguments

- data:

  An `epidist_meta_model` object.

## Value

A numeric vector of Cholesky factor entries.
