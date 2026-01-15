# Convert linelist data to aggregate format

This method takes an `epidist_linelist_data` object (see
[`as_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.md))
and aggregates it by counting unique combinations of the required time
variables (primary/secondary event times and observation time) and any
additional variables specified in `by`. The result is a more compact
representation of the same data where each row represents multiple
identical observations with the count stored in the `n` column.

## Usage

``` r
# S3 method for class 'epidist_linelist_data'
as_epidist_aggregate_data(data, by = NULL, ...)
```

## Arguments

- data:

  The data to convert

- by:

  Character vector of additional variables to stratify by, beyond the
  required time variables.

- ...:

  Additional arguments passed to methods

## See also

Other aggregate_data:
[`as_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.md),
[`as_epidist_aggregate_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.data.frame.md),
[`as_epidist_aggregate_data.default()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.default.md),
[`assert_epidist.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_aggregate_data.md),
[`is_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/is_epidist_aggregate_data.md),
[`new_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/new_epidist_aggregate_data.md)

## Examples

``` r
# Default stratification by required time variables only
sierra_leone_ebola_data |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ) |>
  as_epidist_aggregate_data()
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).
#> # A tibble: 2,453 × 6
#>    ptime_lwr ptime_upr stime_lwr stime_upr obs_time     n
#>        <dbl>     <dbl>     <dbl>     <dbl>    <dbl> <int>
#>  1         0         1         5         6      484     1
#>  2         2         3         7         8      484     2
#>  3         3         4         8         9      484     4
#>  4         4         5         9        10      484     6
#>  5         8         9        13        14      484     1
#>  6         9        10        14        15      484     3
#>  7        11        12        16        17      484     7
#>  8        12        13        17        18      484     7
#>  9        13        14        18        19      484     1
#> 10        13        14        20        21      484     1
#> # ℹ 2,443 more rows

# Additional stratification by other variables
sierra_leone_ebola_data |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ) |>
  as_epidist_aggregate_data(by = "age")
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).
#> # A tibble: 7,667 × 7
#>    ptime_lwr ptime_upr stime_lwr stime_upr obs_time     n   age
#>        <dbl>     <dbl>     <dbl>     <dbl>    <dbl> <int> <dbl>
#>  1         0         1         5         6      484     1    20
#>  2         2         3         7         8      484     1    42
#>  3         2         3         7         8      484     1    45
#>  4         3         4         8         9      484     1    15
#>  5         3         4         8         9      484     1    19
#>  6         3         4         8         9      484     1    50
#>  7         3         4         8         9      484     1    55
#>  8         4         5         9        10      484     1     8
#>  9         4         5         9        10      484     1    27
#> 10         4         5         9        10      484     1    38
#> # ℹ 7,657 more rows
```
