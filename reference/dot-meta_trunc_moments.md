# Summaries of a right truncated delay distribution

Under an accrual design the quadrature is reweighted by the follow up
available to each delay before the moments are taken, which is exact for
a study that adjusted for censoring because the weight then applies to
the delay itself.

## Usage

``` r
.meta_trunc_moments(
  dist,
  args,
  lower = 0,
  cutoff,
  growth_rate = 0,
  accrual = 0L,
  n_quad = .meta_n_quad()
)
```

## Arguments

- dist:

  A `primarycensored` distribution function name.

- args:

  A named list of distribution parameters.

- lower:

  The study's minimum delay (its left truncation point).

- cutoff:

  The grid cutoff, either the study observation time or `max_delay`.

- growth_rate:

  The exponential growth rate of primary events.

- accrual:

  1 to apply the accrual weight, 0 otherwise.

- n_quad:

  The number of quadrature intervals, an even number. Rows built by
  [`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md)
  carry the number chosen for their study by
  [`.estimates_n_quad()`](https://epidist.epinowcast.org/reference/dot-estimates_n_quad.md).

## Value

A named numeric vector with elements `mean`, `sd`, `kurtosis` and
`skewness`.
