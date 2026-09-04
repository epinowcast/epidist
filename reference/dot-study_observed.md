# Whether a study that did not adjust for right truncation saw each case

A cohort study bounds the underlying delay by its observation time, so a
study on the discrete grid keeps a case only if the whole window its
delay fell in is below the cutoff, and a midpoint primary study keeps a
case if its uniform single interval delay is. An accrual study keeps the
cases whose secondary event fell before its calendar stop.

## Usage

``` r
.study_observed(
  cases,
  measured,
  cens_adjusted,
  trunc_design,
  relative_obs_time,
  swindow
)
```

## Arguments

- cases:

  The tibble of
  [`.study_cases()`](https://epidist.epinowcast.org/reference/dot-study_cases.md).

- measured:

  The measured delays of
  [`.study_measured_delay()`](https://epidist.epinowcast.org/reference/dot-study_measured_delay.md).

- cens_adjusted:

  The censoring adjustment code.

- trunc_design:

  The truncation design.

- relative_obs_time:

  The study observation time.

## Value

A logical vector.
