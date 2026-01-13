# Diagnostics for `epidist_fit` models

This function computes diagnostics to assess the quality of a fitted
model. When the fitting algorithm used is `"sampling"` (HMC) then the
output of `epidist_diagnostics` is a `data.frame` containing:

- `time`: the total time taken to fit all chains

- `samples`: the total number of samples across all chains

- `max_rhat`: the highest value of the Gelman-Rubin statistic

- `divergent_transitions`: the total number of divergent transitions

- `per_divergent_transitions`: the proportion of samples which had
  divergent transitions

- `max_treedepth`: the highest value of the treedepth HMC parameter

- `no_at_max_treedepth`: the number of samples which attained the
  `max_treedepth`

- `per_at_max_treedepth`: the proportion of samples which attained the
  `max_treedepth`

## Usage

``` r
epidist_diagnostics(fit)
```

## Arguments

- fit:

  A fitted model of class `epidist_fit`

## Details

When the fitting algorithm is not `"sampling"` (see
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html) for
other possible algorithms) then diagnostics are yet to be implemented.

## Examples

``` r
fit <- sierra_leone_ebola_data |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ) |>
  as_epidist_aggregate_data() |>
  as_epidist_marginal_model() |>
  epidist(chains = 2, cores = 2, refresh = ifelse(interactive(), 250, 0))
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).
#> ! Setting 2394 observation times beyond 98 (=2x max delay) to Inf. This
#>   improves model efficiency by reducing unique observation times while
#>   maintaining model accuracy as these times should have negligible impact.
#> Warning: Found infinite values in the data, which may cause issues for Stan.
#> ℹ Data summarised by unique combinations of:
#> * Model variables: delay bounds, observation time, and primary censoring window
#> ! Reduced from 2453 to 272 rows.
#> ℹ This should improve model efficiency with no loss of information.
#> Warning: Found infinite values in the data, which may cause issues for Stan.
#> Warning: Found infinite values in the data, which may cause issues for Stan.
#> Compiling Stan program...
#> Start sampling
epidist_diagnostics(fit)
#> # A tibble: 1 × 8
#>    time samples max_rhat divergent_transitions per_divergent_transitions
#>   <dbl>   <dbl>    <dbl>                 <dbl>                     <dbl>
#> 1  9.92    2000     1.00                     0                         0
#> # ℹ 3 more variables: max_treedepth <dbl>, no_at_max_treedepth <int>,
#> #   per_at_max_treedepth <dbl>
```
