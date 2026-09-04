# The delay a study measured from each case

The delay a study measured from each case

## Usage

``` r
.study_measured_delay(cases, cens_adjusted, pwindow, swindow)
```

## Arguments

- cases:

  The tibble of
  [`.study_cases()`](https://epidist.epinowcast.org/reference/dot-study_cases.md).

- cens_adjusted:

  The censoring adjustment code.

- pwindow, swindow:

  The primary and secondary window widths.

## Value

A numeric vector of measured delays.
