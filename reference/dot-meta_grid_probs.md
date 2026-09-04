# The continuity corrected cohort grid distribution function at several delays

The vectorised form of
[`.meta_grid_prob()`](https://epidist.epinowcast.org/reference/dot-meta_grid_prob.md)
for a cohort study. Every cell edge a set of reported quantiles needs is
evaluated in one call, because a call to
[`primarycensored::pprimarycensored()`](https://primarycensored.epinowcast.org/reference/pprimarycensored.html)
costs the same whether it is given one delay or a hundred.

## Usage

``` r
.meta_grid_probs(
  y,
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  swindow,
  growth_rate
)
```

## Arguments

- y:

  A numeric vector of delays.

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

- growth_rate:

  The exponential growth rate of primary events.

## Value

A numeric vector of probabilities, or infinities if the grid mass
underflows to zero.
