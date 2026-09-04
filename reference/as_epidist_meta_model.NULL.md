# The meta model method for summary estimates only

Used when no individual level data is available and only the `estimates`
argument is supplied. It takes no `primary` argument, because summary
rows tilt the primary event by the `growth_rate` metadata of their study
rather than by an estimated parameter. Passing one is an error.

## Usage

``` r
# S3 method for class '`NULL`'
as_epidist_meta_model(data = NULL, estimates = NULL, ...)
```

## Arguments

- data:

  `NULL`.

- estimates:

  An `epidist_estimates_data` object of published summary estimates, or
  `NULL`.

- ...:

  Additional arguments passed to methods.

## See also

Other meta_model:
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md),
[`as_epidist_meta_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_aggregate_data.md),
[`as_epidist_meta_model.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_estimates_data.md),
[`as_epidist_meta_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_linelist_data.md),
[`assert_epidist.epidist_meta_model()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_meta_model.md),
[`epidist_family_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.epidist_meta_model.md),
[`epidist_formula_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_meta_model.md),
[`epidist_model_prior.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_model_prior.epidist_meta_model.md),
[`epidist_newdata.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_meta_model.md),
[`epidist_transform_data_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_meta_model.md),
[`is_epidist_meta_model()`](https://epidist.epinowcast.org/reference/is_epidist_meta_model.md),
[`new_epidist_meta_model()`](https://epidist.epinowcast.org/reference/new_epidist_meta_model.md)

## Examples

``` r
estimates <- as_epidist_estimates_data(
  data.frame(
    study = c("A", "A"),
    type = c("mean", "sd"),
    value = c(7.5, 3.6),
    n = c(120, 120),
    relative_obs_time = c(20, 20),
    trunc_adjusted = c(FALSE, FALSE),
    cens_adjusted = c(0, 0)
  )
)
#> ℹ No `pwindow` column supplied, assuming a censoring window of 1 (daily
#>   reporting) for every study.
#> ℹ No `swindow` column supplied, assuming a censoring window of 1 (daily
#>   reporting) for every study.
#> ℹ No trunc_design column supplied, assuming every study that did not adjust for
#>   right truncation followed a cohort with a common observation time rather than
#>   accruing primary events up to a calendar collection stop.
#> ℹ No max_delay column supplied, using the delay beyond which 1% of the second
#>   moment of a lognormal matched to each study's summaries lies (at least 10 and
#>   at most twenty times the largest reported value, in whole secondary windows)
#>   as the grid cutoff, or five times the largest reported value where nothing
#>   can be matched. Raise it if the delay has a longer tail than that, and lower
#>   it to speed up fitting.
as_epidist_meta_model(estimates = estimates)
#> # A tibble: 1 × 20
#>   delay_lwr     n obs_type study_n trunc_adjusted trunc_design cens_adjusted
#>       <int> <dbl>    <int>   <int>          <int>        <int>         <int>
#> 1         0     1        5     120              0            0             0
#> # ℹ 13 more variables: group_start <int>, group_len <int>, chol_start <int>,
#> #   n_quad <int>, relative_obs_time <dbl>, pwindow <dbl>, swindow <dbl>,
#> #   delay_upr <dbl>, delay_min <dbl>, report_se <dbl>, quantile_p <dbl>,
#> #   growth_rate <dbl>, study <chr>
```
