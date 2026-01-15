# Create an epidist_aggregate_data object from vectors of event times

This method takes vectors of event times (primary/secondary event times
and observation time) along with counts and creates an
`epidist_aggregate_data` object. This format is useful when working with
pre-aggregated data where each row represents multiple identical
observations with the count stored in the `n` column. Internally it
makes use of
[`as_epidist_linelist_data.default()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.default.md)
to convert the data to a linelist format before adding the count column.
See the other methods for other data input options.

## Usage

``` r
# Default S3 method
as_epidist_aggregate_data(
  data,
  n = NULL,
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

- n:

  An integerish vector containing the counts for each row. Must be the
  same length as the input data vector.

- ptime_upr:

  Numeric vector giving upper bounds for primary times.

- stime_lwr, stime_upr:

  Numeric vectors giving lower and upper bounds for secondary times.

- obs_time:

  Numeric vector giving observation times.

- ...:

  Additional columns to add to the epidist_linelist_data object.

## See also

Other aggregate_data:
[`as_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.md),
[`as_epidist_aggregate_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.data.frame.md),
[`as_epidist_aggregate_data.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.epidist_linelist_data.md),
[`assert_epidist.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_aggregate_data.md),
[`is_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/is_epidist_aggregate_data.md),
[`new_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/new_epidist_aggregate_data.md)

## Examples

``` r
as_epidist_aggregate_data(
  data = c(1, 2, 3),
  ptime_upr = c(2, 3, 4),
  stime_lwr = c(3, 4, 5),
  stime_upr = c(4, 5, 6),
  obs_time = c(5, 6, 7),
  n = c(1, 2, 3)
)
#> # A tibble: 3 × 6
#>   ptime_lwr ptime_upr stime_lwr stime_upr obs_time     n
#>       <dbl>     <dbl>     <dbl>     <dbl>    <dbl> <dbl>
#> 1         1         2         3         4        5     1
#> 2         2         3         4         5        6     2
#> 3         3         4         5         6        7     3
```
