# The cases of a simulated line list with their exact event times

The cases of a simulated line list with their exact event times

## Usage

``` r
.study_cases(data)
```

## Arguments

- data:

  An `epidist_linelist_data` object built from simulated event times,
  with the exact `ptime` and `stime` columns kept by
  [`simulate_dates()`](https://epidist.epinowcast.org/reference/simulate_dates.md)
  when `keep_times = TRUE`. Every case must use the same primary window
  and the same secondary window.

## Value

A tibble with the exact times checked against their windows.
