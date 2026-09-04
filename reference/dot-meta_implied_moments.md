# The summaries a study using a given procedure would report

Forward models the summaries that a study would converge to given the
biases in its estimation procedure. See
[`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md)
for what the adjustment codes mean.

## Usage

``` r
.meta_implied_moments(
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

A named numeric vector with elements `mean`, `sd`, `kurtosis` and
`skewness`.

## Details

Under the uniform single interval approximation (`cens_adjusted` of 2)
the study summarised the delay plus the uncorrected primary event
offset. Where the study also adjusted for right truncation and the
primary events were uniform within their window this is the analytic
convolution, which adds `pwindow / 2` to the mean and `pwindow^2 / 12`
to the variance. Otherwise the moments of the primary censored delay,
truncated at `cutoff`, are used directly.

A study that adjusted for right truncation and counted only delays above
`lower` has its analytic moments left truncated by
[`.meta_left_moments()`](https://epidist.epinowcast.org/reference/dot-meta_left_moments.md),
so they do not depend on `cutoff`. A study whose primary events were not
uniform within their window has no analytic moments, so it is truncated
at `cutoff` by quadrature instead.

Under midpoint imputation (`cens_adjusted` of 3) the study assigned each
delay to the centre of the interval it was observed in, so the estimand
is the naive discrete grid shifted up by `swindow / 2`. The shift moves
the mean and leaves every central moment unchanged.

Under midpoint imputation with a uniform interval (`cens_adjusted` of 4)
the study placed the primary event at the midpoint of its window instead
of at its lower edge, so the estimand is that of `cens_adjusted` of 2
shifted down by `pwindow / 2`. Both midpoint codes are evaluated by
calling the code they shift. See
[`.meta_cens_base()`](https://epidist.epinowcast.org/reference/dot-meta_cens_base.md)
and
[`.meta_cens_shift()`](https://epidist.epinowcast.org/reference/dot-meta_cens_shift.md).
