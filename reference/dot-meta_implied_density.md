# The density of the biased estimand at a reported quantile value

Used to convert a quantile standard error reported on the delay scale to
the cumulative probability scale the model works on, by the delta
method. For a discrete estimand the density is the mass of the grid cell
the value falls in divided by the grid spacing, which is exactly the
slope of the continuity corrected distribution function there. For a
continuous estimand it is the closed form density of the estimand over
its truncation normaliser, or, under an accrual design, the slope of the
interpolated distribution function. A primary censored delay with a non
uniform primary event has no closed form density, so it falls back to a
central difference.

## Usage

``` r
.meta_implied_density(
  y,
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  swindow,
  trunc_adjusted,
  cens_adjusted,
  growth_rate,
  trunc_design = 0L,
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

- pwindow, swindow:

  The primary and secondary censoring window widths.

- trunc_adjusted:

  1 if the study adjusted for right truncation, 0 otherwise.

- cens_adjusted:

  The censoring adjustment code, one of 0, 1, 2, 3, or 4.

- growth_rate:

  The exponential growth rate of primary events.

- trunc_design:

  0 for a cohort design, 1 for an accrual design.

- n_quad:

  The number of quadrature intervals, an even number. Rows built by
  [`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md)
  carry the number chosen for their study by
  [`.estimates_n_quad()`](https://epidist.epinowcast.org/reference/dot-estimates_n_quad.md).

## Value

A density on the delay scale, or `Inf` if the grid mass or the
distribution function underflows to zero, which forces a `-Inf` log
likelihood rather than a `NaN` one.
