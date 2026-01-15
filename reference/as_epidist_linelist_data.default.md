# Create an epidist_linelist_data object from vectors of event times

This method takes vectors of event times (primary/secondary event times
and observation time) and creates an `epidist_linelist_data` object.
This format is useful when working with individual-level data where each
row represents a single observation. See the other methods for other
data input options.

## Usage

``` r
# Default S3 method
as_epidist_linelist_data(
  data,
  ptime_upr = NULL,
  stime_lwr = NULL,
  stime_upr = NULL,
  obs_time = NULL,
  ...
)
```

## Arguments

- data:

  Numeric vector giving lower bounds for primary times.

- ptime_upr:

  Numeric vector giving upper bounds for primary times.

- stime_lwr, stime_upr:

  Numeric vectors giving lower and upper bounds for secondary times.

- obs_time:

  Numeric vector giving observation times.

- ...:

  Additional columns to add to the epidist_linelist_data object.

## See also

Other linelist_data:
[`as_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.md),
[`as_epidist_linelist_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.data.frame.md),
[`as_epidist_linelist_data.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.epidist_aggregate_data.md),
[`assert_epidist.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_linelist_data.md),
[`is_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/is_epidist_linelist_data.md),
[`new_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/new_epidist_linelist_data.md)

## Examples

``` r
as_epidist_linelist_data(
  data = c(1, 2, 3),
  ptime_upr = c(2, 3, 4),
  stime_lwr = c(3, 4, 5),
  stime_upr = c(4, 5, 6),
  obs_time = c(5, 6, 7)
)
#> # A tibble: 3 × 5
#>   ptime_lwr ptime_upr stime_lwr stime_upr obs_time
#>       <dbl>     <dbl>     <dbl>     <dbl>    <dbl>
#> 1         1         2         3         4        5
#> 2         2         3         4         5        6
#> 3         3         4         5         6        7
```
