# Combine `epidist_estimates_data` objects from several studies

Each element is coerced on its own and the results are bound into one
object. Combining is associative, so contributions can be assembled in
any order and in any grouping. The advisory checks of the Checks section
of
[`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md)
run once, on the combined object, so each message names every study it
applies to.

## Usage

``` r
# S3 method for class 'list'
as_epidist_estimates_data(data, advise = TRUE, ...)
```

## Arguments

- data:

  The data to convert

- advise:

  Whether to run the advisory checks of the Checks section and message
  about the studies they flag. Defaults to `TRUE`. The list method sets
  it to `FALSE` for each element and runs the checks once on the
  combined object.

- ...:

  Passed to the method used for each element.

## See also

Other estimates_data:
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md),
[`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md),
[`as_epidist_estimates_data.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.epidist_estimates_data.md),
[`as_epidist_estimates_data.epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.epidist_multivariate.md),
[`assert_epidist.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_estimates_data.md),
[`epidist_estimates_parameters()`](https://epidist.epinowcast.org/reference/epidist_estimates_parameters.md),
[`epidist_estimates_summaries()`](https://epidist.epinowcast.org/reference/epidist_estimates_summaries.md),
[`is_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/is_epidist_estimates_data.md),
[`new_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/new_epidist_estimates_data.md)

## Examples

``` r
as_epidist_estimates_data(list(
  epidist_estimates_summaries(
    "A",
    mean = 7.5, sd = 3.6, n = 120, trunc_adjusted = TRUE
  ),
  epidist_estimates_summaries(
    "B",
    mean = 6.9, n = 80, relative_obs_time = 20, trunc_adjusted = FALSE
  )
))
#> ℹ No `pwindow` column supplied, assuming a censoring window of 1 (daily
#>   reporting) for every study.
#> ℹ No `swindow` column supplied, assuming a censoring window of 1 (daily
#>   reporting) for every study.
#> ℹ No relative_obs_time column supplied, assuming no observation time limit (no
#>   right truncation) for every study.
#> ℹ No cens_adjusted column supplied, assuming every study used naive integer
#>   date differences without a censoring adjustment.
#> ℹ No max_delay column supplied, using the delay beyond which 1% of the second
#>   moment of a lognormal matched to each study's summaries lies (at least 10 and
#>   at most twenty times the largest reported value, in whole secondary windows)
#>   as the grid cutoff, or five times the largest reported value where nothing
#>   can be matched. Raise it if the delay has a longer tail than that, and lower
#>   it to speed up fitting.
#> ℹ No `pwindow` column supplied, assuming a censoring window of 1 (daily
#>   reporting) for every study.
#> ℹ No `swindow` column supplied, assuming a censoring window of 1 (daily
#>   reporting) for every study.
#> ℹ No trunc_design column supplied, assuming every study that did not adjust for
#>   right truncation followed a cohort with a common observation time rather than
#>   accruing primary events up to a calendar collection stop.
#> ℹ No cens_adjusted column supplied, assuming every study used naive integer
#>   date differences without a censoring adjustment.
#> ℹ No max_delay column supplied, using the delay beyond which 1% of the second
#>   moment of a lognormal matched to each study's summaries lies (at least 10 and
#>   at most twenty times the largest reported value, in whole secondary windows)
#>   as the grid cutoff, or five times the largest reported value where nothing
#>   can be matched. Raise it if the delay has a longer tail than that, and lower
#>   it to speed up fitting.
#> # A tibble: 3 × 16
#>   study type  value    se     n     p pwindow swindow relative_obs_time
#>   <chr> <chr> <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>             <dbl>
#> 1 A     mean    7.5    NA   120    NA       1       1               Inf
#> 2 A     sd      3.6    NA   120    NA       1       1               Inf
#> 3 B     mean    6.9    NA    80    NA       1       1                20
#> # ℹ 7 more variables: trunc_adjusted <lgl>, trunc_design <chr>,
#> #   cens_adjusted <int>, delay_min <dbl>, growth_rate <dbl>, max_delay <dbl>,
#> #   mvn_id <chr>
```
