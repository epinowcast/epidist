# The latent model method for `epidist_aggregate_data` objects

This method converts aggregate data to a latent model format by first
converting it to linelist format using
[`as_epidist_linelist_data.epidist_aggregate_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_linelist_data.epidist_aggregate_data.md)
and then passing it to
[`as_epidist_latent_model.epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_latent_model.epidist_linelist_data.md).
This ensures that the counts in the aggregate data are properly expanded
into individual observations before fitting the latent model.

## Usage

``` r
# S3 method for class 'epidist_aggregate_data'
as_epidist_latent_model(data, ...)
```

## Arguments

- data:

  An `epidist_aggregate_data` object

- ...:

  Not used in this method.

## See also

Other latent_model:
[`as_epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/as_epidist_latent_model.md),
[`as_epidist_latent_model.epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_latent_model.epidist_linelist_data.md),
[`epidist_family_model.epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/epidist_family_model.epidist_latent_model.md),
[`epidist_formula_model.epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/epidist_formula_model.epidist_latent_model.md),
[`epidist_model_prior.epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/epidist_model_prior.epidist_latent_model.md),
[`is_epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/is_epidist_latent_model.md),
[`new_epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/new_epidist_latent_model.md)

## Examples

``` r
sierra_leone_ebola_data |>
  dplyr::count(date_of_symptom_onset, date_of_sample_tested) |>
  as_epidist_aggregate_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested",
    n = "n"
  ) |>
  as_epidist_latent_model()
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).
#> # A tibble: 8,358 × 16
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
#> # ℹ 9 more variables: pdate_upr <date>, sdate_upr <date>, obs_date <date>,
#> #   relative_obs_time <dbl>, pwindow <dbl>, woverlap <dbl>, swindow <dbl>,
#> #   delay <dbl>, .row_id <int>
```
