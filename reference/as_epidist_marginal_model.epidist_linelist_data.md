# The marginal model method for `epidist_linelist_data` objects

This method converts linelist data to a marginal model format by
calculating delays between primary and secondary events, along with
observation times and censoring windows. The likelihood used is imported
from the [primarycensored](https://primarycensored.epinowcast.org/)
package which handles censoring in both primary and secondary events as
well as truncation due to observation times. In principle, this method
should be more accurate and more computationally efficient than the
latent model
([`as_epidist_latent_model()`](https://epidist.epinowcast.org/reference/as_epidist_latent_model.md))
approach in most settings except when the number of unique strata
approaches the number of observations.

## Usage

``` r
# S3 method for class 'epidist_linelist_data'
as_epidist_marginal_model(data, obs_time_threshold = 2, weight = NULL, ...)
```

## Arguments

- data:

  An `epidist_linelist_data` object

- obs_time_threshold:

  Ratio used to determine threshold for setting relative observation
  times to Inf. Observation times greater than `obs_time_threshold`
  times the maximum delay will be set to Inf to improve model efficiency
  by reducing the number of unique observation times. Default is 2.

- weight:

  A column name to use for weighting the data in the likelihood. Default
  is NULL. Internally this is used to define the 'n' column of the
  returned object.

- ...:

  Not used in this method.

## Details

When a formula is specified in
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md), the
data will be transformed using
[`epidist_transform_data_model.epidist_marginal_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_marginal_model.md)
to prepare it for model fitting. This transformation summarises the data
by counting unique combinations of delays, observation times, censoring
windows and any variables in the model formula.

## See also

Other marginal_model:
[`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.md),
[`as_epidist_marginal_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.epidist_aggregate_data.md),
[`epidist_family_model.epidist_marginal_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.epidist_marginal_model.md),
[`epidist_formula_model.epidist_marginal_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_marginal_model.md),
[`epidist_transform_data_model.epidist_marginal_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_marginal_model.md),
[`is_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/is_epidist_marginal_model.md),
[`new_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/new_epidist_marginal_model.md)

## Examples

``` r
sierra_leone_ebola_data |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ) |>
  as_epidist_marginal_model()
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).
#> ! Setting 8294 observation times beyond 98 (=2x max delay) to Inf. This
#>   improves model efficiency by reducing unique observation times while
#>   maintaining model accuracy as these times should have negligible impact.
#> # A tibble: 8,358 × 22
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
#> # ℹ 13 more variables: sdate_lwr <date>, district <chr>, chiefdom <chr>,
#> #   pdate_upr <date>, sdate_upr <date>, obs_date <date>, pwindow <dbl>,
#> #   swindow <dbl>, relative_obs_time <dbl>, orig_relative_obs_time <dbl>,
#> #   delay_lwr <dbl>, delay_upr <dbl>, n <dbl>
```
