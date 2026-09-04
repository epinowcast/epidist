# Create an `epidist_multivariate` object from a matrix of draws

Rows are draws and columns are parameters, describing a single point.

## Usage

``` r
# S3 method for class 'matrix'
as_epidist_multivariate(draws, params = NULL, ...)
```

## Arguments

- draws:

  Draws of the parameters. See the methods for supported formats.

- params:

  A character vector naming the columns of `draws`. Defaults to the
  column names of `draws`.

- ...:

  Not used in this method.

## See also

Other multivariate:
[`as_epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.md),
[`as_epidist_multivariate.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.data.frame.md),
[`assert_epidist.epidist_multivariate()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_multivariate.md),
[`is_epidist_multivariate()`](https://epidist.epinowcast.org/reference/is_epidist_multivariate.md),
[`new_epidist_multivariate()`](https://epidist.epinowcast.org/reference/new_epidist_multivariate.md),
[`print.epidist_multivariate()`](https://epidist.epinowcast.org/reference/print.epidist_multivariate.md),
[`vcov.epidist_multivariate()`](https://epidist.epinowcast.org/reference/vcov.epidist_multivariate.md)

## Examples

``` r
set.seed(1)
draws <- cbind(mean = rnorm(500, 7.5, 0.3), sd = rnorm(500, 3.6, 0.2))
as_epidist_multivariate(draws)
#> A multivariate representation of 2 parameters at 1 index point.
#>     mean       sd 
#> 7.506793 3.590812 
#>              [,1]         [,2]
#> [1,]  0.092159903 -0.002646723
#> [2,] -0.002646723  0.044715697
```
