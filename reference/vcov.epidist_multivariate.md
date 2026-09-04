# The covariance matrix of an `epidist_multivariate` object

The covariance matrix of an `epidist_multivariate` object

## Usage

``` r
# S3 method for class 'epidist_multivariate'
vcov(object, ...)
```

## Arguments

- object:

  An `epidist_multivariate` object.

- ...:

  Not used.

## Value

The covariance matrix.

## See also

Other multivariate:
[`as_epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.md),
[`as_epidist_multivariate.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.data.frame.md),
[`as_epidist_multivariate.matrix()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.matrix.md),
[`assert_epidist.epidist_multivariate()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_multivariate.md),
[`is_epidist_multivariate()`](https://epidist.epinowcast.org/reference/is_epidist_multivariate.md),
[`new_epidist_multivariate()`](https://epidist.epinowcast.org/reference/new_epidist_multivariate.md),
[`print.epidist_multivariate()`](https://epidist.epinowcast.org/reference/print.epidist_multivariate.md)

## Examples

``` r
vcov(new_epidist_multivariate(
  value = c(mean = 7.5, sd = 3.6),
  vcov = matrix(c(0.09, 0.02, 0.02, 0.04), nrow = 2),
  params = c("mean", "sd")
))
#>      [,1] [,2]
#> [1,] 0.09 0.02
#> [2,] 0.02 0.04
```
