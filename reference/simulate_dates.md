# Convert simulated event times to dates

Takes the continuous event times produced by
[`simulate_gillespie()`](https://epidist.epinowcast.org/reference/simulate_gillespie.md)
and
[`simulate_secondary()`](https://epidist.epinowcast.org/reference/simulate_secondary.md)
and returns the dates an analyst would actually receive. Event times are
floored to their reporting window, so each event is known only by the
window it fell in, and are then offset from `outbreak_start_date`.

## Usage

``` r
simulate_dates(
  data,
  outbreak_start_date = as.Date("2024-01-01"),
  primary_window = 1,
  secondary_window = primary_window,
  obs_time = NULL,
  keep_times = FALSE
)
```

## Arguments

- data:

  A `data.frame` with numeric `ptime` and `stime` columns, as returned
  by
  [`simulate_secondary()`](https://epidist.epinowcast.org/reference/simulate_secondary.md).

- outbreak_start_date:

  The date the outbreak started, corresponding to time zero.

- primary_window:

  Width of the primary event reporting window in days. Either a single
  value used for every observation, or one value per row of `data`. The
  default of 1 gives daily reporting. Use 7 for weekly.

- secondary_window:

  Width of the secondary event reporting window in days, in the same
  form as `primary_window`. Defaults to `primary_window`, so the two
  events share a reporting interval unless you say otherwise.

- obs_time:

  Optional numeric observation time, in the same units as `ptime` and
  `stime`. When supplied an `obs_date` column is added. When `NULL`, the
  default, no observation date is added and
  [`as_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.md)
  falls back to the day after the last secondary event.

- keep_times:

  Whether to keep the underlying numeric times. Useful when comparing
  estimates against the values used to simulate.

## Value

A `data.frame` with `pdate_lwr`, `pdate_upr`, `sdate_lwr` and
`sdate_upr` columns, and `obs_date` when `obs_time` is supplied.

## Details

The returned columns are named to match
[`as_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.md),
so the output can be passed straight to it.

## See also

Other simulate:
[`simulate_exponential_cases()`](https://epidist.epinowcast.org/reference/simulate_exponential_cases.md),
[`simulate_gillespie()`](https://epidist.epinowcast.org/reference/simulate_gillespie.md),
[`simulate_secondary()`](https://epidist.epinowcast.org/reference/simulate_secondary.md),
[`simulate_uniform_cases()`](https://epidist.epinowcast.org/reference/simulate_uniform_cases.md)

## Examples

``` r
simulate_gillespie(seed = 1) |>
  simulate_secondary(meanlog = 1.8, sdlog = 0.5) |>
  simulate_dates(outbreak_start_date = as.Date("2024-02-01")) |>
  head()
#>   case  pdate_lwr  pdate_upr  sdate_lwr  sdate_upr
#> 1    1 2024-02-01 2024-02-02 2024-02-09 2024-02-10
#> 2    2 2024-02-01 2024-02-02 2024-02-06 2024-02-07
#> 3    3 2024-02-01 2024-02-02 2024-02-09 2024-02-10
#> 4    4 2024-02-01 2024-02-02 2024-02-10 2024-02-11
#> 5    5 2024-02-01 2024-02-02 2024-02-08 2024-02-09
#> 6    6 2024-02-01 2024-02-02 2024-02-10 2024-02-11
```
