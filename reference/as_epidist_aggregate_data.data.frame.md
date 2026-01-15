# Create an epidist_aggregate_data object from a data.frame

This method takes a data.frame containing event dates (primary/secondary
event dates and observation date) along with counts and creates an
`epidist_aggregate_data` object. This format is useful when working with
pre-aggregated data where each row represents multiple identical
observations with the count stored in a specified column. Internally it
makes use of
[`as_epidist_linelist_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.data.frame.md)
to convert the data to a linelist format before adding the count column.
See the other methods for other data input options.

## Usage

``` r
# S3 method for class 'data.frame'
as_epidist_aggregate_data(
  data,
  n = NULL,
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

- n:

  A character string giving the name of the column containing the counts
  for each row. If `NULL` then the column `n` must be present in the
  data.

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

Other aggregate_data:
[`as_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.md),
[`as_epidist_aggregate_data.default()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.default.md),
[`as_epidist_aggregate_data.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.epidist_linelist_data.md),
[`assert_epidist.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_aggregate_data.md),
[`is_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/is_epidist_aggregate_data.md),
[`new_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/new_epidist_aggregate_data.md)

## Examples

``` r
sierra_leone_ebola_data |>
  dplyr::count(date_of_symptom_onset, date_of_sample_tested) |>
  as_epidist_aggregate_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested",
    n = "n"
  )
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).
#> # A tibble: 2,453 × 11
#>    ptime_lwr ptime_upr stime_lwr stime_upr obs_time pdate_lwr  sdate_lwr      n
#>        <dbl>     <dbl>     <dbl>     <dbl>    <dbl> <date>     <date>     <int>
#>  1         0         1         5         6      484 2014-05-18 2014-05-23     1
#>  2         2         3         7         8      484 2014-05-20 2014-05-25     2
#>  3         3         4         8         9      484 2014-05-21 2014-05-26     4
#>  4         4         5         9        10      484 2014-05-22 2014-05-27     6
#>  5         8         9        13        14      484 2014-05-26 2014-05-31     1
#>  6         9        10        14        15      484 2014-05-27 2014-06-01     3
#>  7        11        12        16        17      484 2014-05-29 2014-06-03     7
#>  8        12        13        17        18      484 2014-05-30 2014-06-04     7
#>  9        13        14        18        19      484 2014-05-31 2014-06-05     1
#> 10        13        14        20        21      484 2014-05-31 2014-06-07     1
#> # ℹ 2,443 more rows
#> # ℹ 3 more variables: pdate_upr <date>, sdate_upr <date>, obs_date <date>
```
