# The latent model method for `epidist_linelist_data` objects

This method takes an `epidist_linelist_data` object and converts it to a
format suitable for fitting latent variable models. It calculates key
variables needed for the latent variable method described in Park et al.
(2024) and Charniga et al. (2024). This approach adjusts for double
interval censoring and right truncation in the data.

## Usage

``` r
# S3 method for class 'epidist_linelist_data'
as_epidist_latent_model(data, ...)
```

## Arguments

- data:

  An `epidist_linelist_data` object containing individual-level
  observations with primary and secondary event times. See
  [`as_epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_linelist_data.md)
  for details on creating this object.

- ...:

  Not used in this method.

## References

- [Park et al. (2024)](https://doi.org/10.1101/2024.01.12.24301247)

- [Charniga et al. (2024)](https://doi.org/10.1371/journal.pcbi.1012520)

## See also

Other latent_model:
[`as_epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/as_epidist_latent_model.md),
[`as_epidist_latent_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_latent_model.epidist_aggregate_data.md),
[`epidist_family_model.epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/epidist_family_model.epidist_latent_model.md),
[`epidist_formula_model.epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/epidist_formula_model.epidist_latent_model.md),
[`epidist_model_prior.epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/epidist_model_prior.epidist_latent_model.md),
[`is_epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/is_epidist_latent_model.md),
[`new_epidist_latent_model()`](https://epidist.epinowcast.org/dev/reference/new_epidist_latent_model.md)

## Examples

``` r
sierra_leone_ebola_data |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ) |>
  as_epidist_latent_model()
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).
#> # A tibble: 8,358 × 21
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
#> # ℹ 12 more variables: sdate_lwr <date>, district <chr>, chiefdom <chr>,
#> #   pdate_upr <date>, sdate_upr <date>, obs_date <date>,
#> #   relative_obs_time <dbl>, pwindow <dbl>, woverlap <dbl>, swindow <dbl>,
#> #   delay <dbl>, .row_id <int>
```
