# The cumulative probability a study using a given procedure would report

Evaluates the distribution function of the biased estimand at a reported
quantile value. Working on the probability scale avoids inverting the
distribution function, which has no closed form on the discrete grid.

## Usage

``` r
.meta_implied_prob(
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

A probability, or `Inf` if the distribution function underflows to zero
at `cutoff`, which forces a `-Inf` log likelihood rather than a `NaN`
one.

## Details

For a naive study (`cens_adjusted` of 0) the estimand is discrete, so
the continuity corrected grid distribution function of
[`.meta_grid_prob()`](https://epidist.epinowcast.org/reference/dot-meta_grid_prob.md)
is used. Midpoint imputation (`cens_adjusted` of 3) uses the same
function evaluated half a secondary window lower, because the study
shifted every delay up by that amount.

For the uniform single interval approximation (`cens_adjusted` of 2) the
distribution function of the primary censored delay is used, so that it
matches the moments used for reported means and standard deviations.
Midpoint imputation with a uniform interval (`cens_adjusted` of 4) uses
that function evaluated half a primary window higher, because the study
anchored every delay at the centre of the primary window rather than at
its lower edge.
