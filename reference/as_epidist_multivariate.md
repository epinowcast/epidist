# Create an `epidist_multivariate` object

Summarises draws of a set of parameters by their mean vector and
covariance matrix. The result is a multivariate normal approximation to
whatever the draws describe, and it is the only route by which a
covariance between reported quantities reaches
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md).

## Usage

``` r
as_epidist_multivariate(draws, ...)
```

## Arguments

- draws:

  Draws of the parameters. See the methods for supported formats.

- ...:

  Additional arguments passed to methods.

## Details

The draws may be of any parameters. Posterior draws of a delay mean and
standard deviation, of a `meanlog` and `sdlog`, or of a `shape` and a
`scale` all give the same object. Nothing here checks that a parameter
is something a study could report, because that only matters when the
object is converted with
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md).

Draws may also cover a trajectory, with the parameters varying over an
`index`. The elements are then ordered index major and parameter minor,
so a `mean` and an `sd` over two index points give `mean[1]`, `sd[1]`,
`mean[2]`, `sd[2]`. The order is fixed because the covariance is indexed
by it.

There is no method for a vector of individual delays. Resampling delays
manufactures a covariance the study never estimated, which is not what
this represents.

## See also

Other multivariate:
[`as_epidist_multivariate.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.data.frame.md),
[`as_epidist_multivariate.matrix()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.matrix.md),
[`assert_epidist.epidist_multivariate()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_multivariate.md),
[`is_epidist_multivariate()`](https://epidist.epinowcast.org/reference/is_epidist_multivariate.md),
[`new_epidist_multivariate()`](https://epidist.epinowcast.org/reference/new_epidist_multivariate.md),
[`print.epidist_multivariate()`](https://epidist.epinowcast.org/reference/print.epidist_multivariate.md),
[`vcov.epidist_multivariate()`](https://epidist.epinowcast.org/reference/vcov.epidist_multivariate.md)
