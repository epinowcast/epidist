# The naive model method for `epidist_aggregate_data` objects

This method converts aggregate data to a naive model format by passing
it to
[`as_epidist_naive_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.epidist_linelist_data.md)
with the `n` column used as weights. This ensures that the likelihood is
weighted by the counts in the aggregate data.

## Usage

``` r
# S3 method for class 'epidist_aggregate_data'
as_epidist_naive_model(data, ...)
```

## Arguments

- data:

  An `epidist_aggregate_data` object.

- ...:

  Additional arguments passed to methods.

## See also

Other naive_model:
[`as_epidist_naive_model()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.md),
[`as_epidist_naive_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.epidist_linelist_data.md),
[`epidist_formula_model.epidist_naive_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_naive_model.md),
[`epidist_transform_data_model.epidist_naive_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_naive_model.md),
[`is_epidist_naive_model()`](https://epidist.epinowcast.org/reference/is_epidist_naive_model.md),
[`new_epidist_naive_model()`](https://epidist.epinowcast.org/reference/new_epidist_naive_model.md)

## Examples

``` r
sierra_leone_ebola_data |>
  dplyr::count(date_of_symptom_onset, date_of_sample_tested) |>
  as_epidist_aggregate_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested",
    n = "n"
  ) |>
  as_epidist_naive_model()
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).
#> # A tibble: 2,453 × 12
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
#> # ℹ 4 more variables: pdate_upr <date>, sdate_upr <date>, obs_date <date>,
#> #   delay <dbl>
```
