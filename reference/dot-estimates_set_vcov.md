# Attach reported covariance matrices to an `epidist_estimates_data` object

The matrices are held alongside the data rather than in it, because a
covariance matrix spans several rows. They are keyed by the `mvn_id`
column, which names the multivariate object the rows came from, so one
study may contribute more than one. Their Cholesky factors are built
once by
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md)
and passed to Stan, so the sampler never decomposes them.

## Usage

``` r
.estimates_set_vcov(data, vcov)
```

## Arguments

- data:

  An `epidist_estimates_data` object.

- vcov:

  A named list of covariance matrices, or `NULL`.

## Value

The input with the matrices attached.

## Details

Only
[`as_epidist_estimates_data.epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.epidist_multivariate.md)
writes to this.
