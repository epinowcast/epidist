# Class constructor for `epidist_multivariate` objects

Use this where a study published a mean vector and a covariance matrix
directly, rather than draws they can be computed from. Converting the
object with a `family`, which needs the draws themselves, is then
unavailable.

## Usage

``` r
new_epidist_multivariate(
  value,
  vcov,
  params,
  index = 1,
  n_draws = NA_integer_,
  draws = NULL
)
```

## Arguments

- value:

  A named numeric vector of the mean of each element.

- vcov:

  The covariance matrix of `value`.

- params:

  A character vector of the parameter names, in order.

- index:

  The trajectory points, in order. Defaults to a single point.

- n_draws:

  The number of draws `value` and `vcov` were computed from, or `NA`
  where they were published directly.

- draws:

  A matrix of draws with one row per draw and one column per element of
  `value`, or `NULL`.

## Value

An object of class `epidist_multivariate`.

## See also

Other multivariate:
[`as_epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.md),
[`as_epidist_multivariate.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.data.frame.md),
[`as_epidist_multivariate.matrix()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.matrix.md),
[`assert_epidist.epidist_multivariate()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_multivariate.md),
[`is_epidist_multivariate()`](https://epidist.epinowcast.org/reference/is_epidist_multivariate.md),
[`print.epidist_multivariate()`](https://epidist.epinowcast.org/reference/print.epidist_multivariate.md),
[`vcov.epidist_multivariate()`](https://epidist.epinowcast.org/reference/vcov.epidist_multivariate.md)

## Examples

``` r
new_epidist_multivariate(
  value = c(mean = 7.5, sd = 3.6),
  vcov = matrix(c(0.09, 0.02, 0.02, 0.04), nrow = 2),
  params = c("mean", "sd")
)
#> A multivariate representation of 2 parameters at 1 index point.
#> mean   sd 
#>  7.5  3.6 
#>      [,1] [,2]
#> [1,] 0.09 0.02
#> [2,] 0.02 0.04
```
