# The density of a continuous estimand under an accrual design

The distribution function is interpolated linearly between quadrature
nodes, so its slope on the interval containing the reported value is the
density there.

## Usage

``` r
.meta_accrual_density(
  y,
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  cens_adjusted,
  growth_rate,
  n_quad = .meta_n_quad()
)
```

## Arguments

- y:

  The reported quantile value.

- dist:

  A `primarycensored` distribution function name.

- args:

  A named list of distribution parameters.

- lower:

  The study's minimum delay (its left truncation point).

- cutoff:

  The grid cutoff, either the study observation time or `max_delay`.

- cens_adjusted:

  The censoring adjustment code, one of 0, 1, 2, 3, or 4.

- growth_rate:

  The exponential growth rate of primary events.

- n_quad:

  The number of quadrature intervals, an even number. Rows built by
  [`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md)
  carry the number chosen for their study by
  [`.estimates_n_quad()`](https://epidist.epinowcast.org/reference/dot-estimates_n_quad.md).

## Value

A density on the delay scale.
