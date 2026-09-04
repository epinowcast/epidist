# Summaries of a right truncated primary censored delay distribution

The estimand is the delay plus the primary event offset within its
window, conditioned on falling below the cutoff. Under an accrual design
the quadrature is reweighted by the follow up available to each delay,
offset by half a primary window because the estimand already includes
the primary event offset. See
[`.meta_accrual_reweight()`](https://epidist.epinowcast.org/reference/dot-meta_accrual_reweight.md).

## Usage

``` r
.meta_pcens_trunc_moments(
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  growth_rate,
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
