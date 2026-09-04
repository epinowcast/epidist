# Create an `epidist_multivariate` object from a data frame of draws

[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)
returns exactly this shape, with a `.draw` column, a `.row` column and
one column per parameter, so its output can be passed straight in. Add
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md)
to report the natural scale mean and standard deviation rather than the
distributional parameters.

## Usage

``` r
# S3 method for class 'data.frame'
as_epidist_multivariate(draws, params = NULL, index = NULL, draw = NULL, ...)
```

## Arguments

- draws:

  Draws of the parameters. See the methods for supported formats.

- params:

  A character vector of the columns holding the parameter draws.
  Defaults to every numeric column other than `draw` and `index`. Supply
  it wherever the draws carry more columns than the parameters being
  reported, because parameters that are functions of each other have a
  singular covariance.

- index:

  A string giving the column that identifies the trajectory point.
  Defaults to `".row"` where such a column exists, as
  [`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)
  returns, and otherwise to `NULL`, meaning the draws describe a single
  point.

- draw:

  A string giving the column that identifies the draw. Defaults to
  `".draw"` where such a column exists, and otherwise to `NULL`, meaning
  row order.

- ...:

  Not used in this method.

## See also

Other multivariate:
[`as_epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.md),
[`as_epidist_multivariate.matrix()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.matrix.md),
[`assert_epidist.epidist_multivariate()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_multivariate.md),
[`is_epidist_multivariate()`](https://epidist.epinowcast.org/reference/is_epidist_multivariate.md),
[`new_epidist_multivariate()`](https://epidist.epinowcast.org/reference/new_epidist_multivariate.md),
[`print.epidist_multivariate()`](https://epidist.epinowcast.org/reference/print.epidist_multivariate.md),
[`vcov.epidist_multivariate()`](https://epidist.epinowcast.org/reference/vcov.epidist_multivariate.md)

## Examples

``` r
set.seed(1)
draws <- data.frame(
  mean = rnorm(500, 7.5, 0.3), sd = rnorm(500, 3.6, 0.2)
)
as_epidist_multivariate(draws)
#> A multivariate representation of 2 parameters at 1 index point.
#>     mean       sd 
#> 7.506793 3.590812 
#>              [,1]         [,2]
#> [1,]  0.092159903 -0.002646723
#> [2,] -0.002646723  0.044715697
```
