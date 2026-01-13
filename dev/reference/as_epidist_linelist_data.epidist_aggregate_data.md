# Convert aggregate data to linelist format

This method expands an `epidist_aggregate_data` object into individual
observations by uncounting the `n` column, then converts it to linelist
format using
[`as_epidist_linelist_data.data.frame()`](https://epidist.epinowcast.org/dev/reference/as_epidist_linelist_data.data.frame.md).

## Usage

``` r
# S3 method for class 'epidist_aggregate_data'
as_epidist_linelist_data(data, ...)
```

## Arguments

- data:

  The data to convert

- ...:

  Additional arguments passed to methods

## See also

Other linelist_data:
[`as_epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_linelist_data.md),
[`as_epidist_linelist_data.data.frame()`](https://epidist.epinowcast.org/dev/reference/as_epidist_linelist_data.data.frame.md),
[`as_epidist_linelist_data.default()`](https://epidist.epinowcast.org/dev/reference/as_epidist_linelist_data.default.md),
[`assert_epidist.epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/assert_epidist.epidist_linelist_data.md),
[`is_epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/is_epidist_linelist_data.md),
[`new_epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/new_epidist_linelist_data.md)

## Examples

``` r
sierra_leone_ebola_data |>
  dplyr::count(date_of_symptom_onset, date_of_sample_tested) |>
  as_epidist_aggregate_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested",
    n = "n"
  ) |>
  as_epidist_linelist_data()
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).
#> # A tibble: 8,358 × 10
#>    ptime_lwr ptime_upr stime_lwr stime_upr obs_time pdate_lwr  sdate_lwr 
#>        <dbl>     <dbl>     <dbl>     <dbl>    <dbl> <date>     <date>    
#>  1         0         1         5         6      484 2014-05-18 2014-05-23
#>  2         2         3         7         8      484 2014-05-20 2014-05-25
#>  3         2         3         7         8      484 2014-05-20 2014-05-25
#>  4         3         4         8         9      484 2014-05-21 2014-05-26
#>  5         3         4         8         9      484 2014-05-21 2014-05-26
#>  6         3         4         8         9      484 2014-05-21 2014-05-26
#>  7         3         4         8         9      484 2014-05-21 2014-05-26
#>  8         4         5         9        10      484 2014-05-22 2014-05-27
#>  9         4         5         9        10      484 2014-05-22 2014-05-27
#> 10         4         5         9        10      484 2014-05-22 2014-05-27
#> # ℹ 8,348 more rows
#> # ℹ 3 more variables: pdate_upr <date>, sdate_upr <date>, obs_date <date>
```
