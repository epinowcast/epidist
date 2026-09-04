# The distribution function of a continuous estimand under an accrual design

Interpolates the accrual weighted distribution function of
[`.meta_accrual_nodes()`](https://epidist.epinowcast.org/reference/dot-meta_accrual_nodes.md)
linearly at the reported value.

## Usage

``` r
.meta_accrual_prob(
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

A probability.
