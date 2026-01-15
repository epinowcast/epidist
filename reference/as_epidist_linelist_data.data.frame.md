# Create an epidist_linelist_data object from a data frame with event dates

This method takes a data.frame containing event dates (primary/secondary
event dates and observation date) and creates an `epidist_linelist_data`
object. This format is useful when working with individual-level data
where each row represents a single observation. Internally it converts
dates to numeric times relative to the earliest primary event date and
uses
[`as_epidist_linelist_data.default()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.default.md)
to create the final object. See the other methods for other data input
options.

## Usage

``` r
# S3 method for class 'data.frame'
as_epidist_linelist_data(
  data,
  pdate_lwr = NULL,
  sdate_lwr = NULL,
  pdate_upr = NULL,
  sdate_upr = NULL,
  obs_date = NULL,
  ...
)
```

## Arguments

- data:

  A data.frame containing line list data

- pdate_lwr:

  A string giving the column of `data` containing the primary event
  lower bound as a datetime. Defaults to `NULL` which assumes that the
  variable `pdate_lwr` is present.

- sdate_lwr:

  A string giving the column of `data` containing the secondary event
  lower bound as a datetime. Defaults to `NULL` which assumes that the
  variable `sdate_lwr` is present.

- pdate_upr:

  A string giving the column of `data` containing the primary event
  upper bound as a datetime. If this column exists in the data it will
  be used, otherwise if not supplied then the value of `pdate_lwr` + 1
  day is used.

- sdate_upr:

  A string giving the column of `data` containing the secondary event
  upper bound as a datetime. If this column exists in the data it will
  be used, otherwise if not supplied then the value of `sdate_lwr` + 1
  day is used.

- obs_date:

  A string giving the column of `data` containing the observation time
  as a datetime. Optional, if not supplied then the maximum of
  `sdate_upr` is used.

- ...:

  Additional arguments passed to methods

## See also

Other linelist_data:
[`as_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.md),
[`as_epidist_linelist_data.default()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.default.md),
[`as_epidist_linelist_data.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.epidist_aggregate_data.md),
[`assert_epidist.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_linelist_data.md),
[`is_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/is_epidist_linelist_data.md),
[`new_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/new_epidist_linelist_data.md)

## Examples

``` r
sierra_leone_ebola_data |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  )
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).
#> # A tibble: 8,358 × 15
#>    ptime_lwr ptime_upr stime_lwr stime_upr obs_time    id   age sex   pdate_lwr 
#>        <dbl>     <dbl>     <dbl>     <dbl>    <dbl> <int> <dbl> <chr> <date>    
#>  1         0         1         5         6      484     1    20 Fema… 2014-05-18
#>  2         2         3         7         8      484     2    42 Fema… 2014-05-20
#>  3         2         3         7         8      484     3    45 Fema… 2014-05-20
#>  4         3         4         8         9      484     4    15 Fema… 2014-05-21
#>  5         3         4         8         9      484     5    19 Fema… 2014-05-21
#>  6         3         4         8         9      484     6    55 Fema… 2014-05-21
#>  7         3         4         8         9      484     7    50 Fema… 2014-05-21
#>  8         4         5         9        10      484     8     8 Fema… 2014-05-22
#>  9         4         5         9        10      484     9    54 Fema… 2014-05-22
#> 10         4         5         9        10      484    10    57 Fema… 2014-05-22
#> # ℹ 8,348 more rows
#> # ℹ 6 more variables: sdate_lwr <date>, district <chr>, chiefdom <chr>,
#> #   pdate_upr <date>, sdate_upr <date>, obs_date <date>
```
