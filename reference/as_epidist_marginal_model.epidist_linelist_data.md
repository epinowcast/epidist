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
as_epidist_marginal_model(
  data,
  obs_time_threshold = 2,
  weight = NULL,
  delay_min = NULL,
  ...
)
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

  A column name containing counts of identical linelist items. When
  specified, the user is declaring that rows with the same values
  represent the same observation occurring multiple times. This allows
  for efficient data representation by storing unique patterns with
  their counts rather than repeating identical rows. The marginal model
  will further aggregate these counts based on the formula
  specification. Default is NULL, which assigns a count of 1 to each
  row. Internally this is used to define the 'n' column of the returned
  object.

- delay_min:

  Minimum delay (left truncation point). Can be:

  - `NULL` (default): uses a `delay_min` column from the data if
    present, otherwise defaults to 0 (no left truncation).

  - A numeric scalar: applied to all observations.

  - A column name string: looks up the named column in the data. This is
    passed as the `L` parameter to
    [`primarycensored::dpcens()`](https://primarycensored.epinowcast.org/reference/dprimarycensored.html).

- ...:

  Not used in this method.

## Details

The marginal model performs internal aggregation to optimize
computational efficiency while preserving all statistical information.
If your data already contains repeated observations with identical
characteristics, you can use the `weight` parameter to provide counts of
these duplicates. This allows for more efficient data representation
without any loss of information.

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
#> # A tibble: 8,358 × 23
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
#> # ℹ 14 more variables: sdate_lwr <date>, district <chr>, chiefdom <chr>,
#> #   pdate_upr <date>, sdate_upr <date>, obs_date <date>, pwindow <dbl>,
#> #   swindow <dbl>, relative_obs_time <dbl>, orig_relative_obs_time <dbl>,
#> #   delay_lwr <dbl>, delay_upr <dbl>, n <dbl>, delay_min <dbl>
```
