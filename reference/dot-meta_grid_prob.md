# The continuity corrected distribution function of a discrete delay grid

The step distribution function of the grid is replaced by the version
that interpolates it linearly through the mid points of its cells.
Without this correction a quantile of day resolution data, which must
land on a jump of the step function, biases the implied probability
upwards by several sampling standard errors.

## Usage

``` r
.meta_grid_prob(
  y,
  dist,
  args,
  lower = 0,
  cutoff,
  pwindow,
  swindow,
  growth_rate,
  accrual = 0L
)
```

## Arguments

- y:

  The delay to evaluate the distribution function at.

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

- accrual:

  1 to apply the accrual weight, 0 otherwise.

## Value

A probability, or `Inf` if the grid mass underflows to zero, which
forces a `-Inf` log likelihood rather than a `NaN` one.

## Details

A cohort grid only needs the two cell edges the value falls between, so
it takes the three evaluation shortcut of
[`.meta_grid_edges()`](https://epidist.epinowcast.org/reference/dot-meta_grid_edges.md).
An accrual grid must be built in full.
