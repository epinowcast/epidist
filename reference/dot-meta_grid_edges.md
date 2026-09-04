# The cohort grid distribution function at the two edges of one cell

A cohort grid is normalised by the mass it holds, so its cumulative sum
at cell \\k\\ is \\(F(k \times swindow) - F(L)) / (F(n\_{grid} \times
swindow) - F(L))\\. A reported quantile therefore needs four
distribution function evaluations rather than the whole grid, and three
where the study counted every delay. This does not hold under an accrual
design, which reweights each cell before renormalising and so must keep
the full grid.

## Usage

``` r
.meta_grid_edges(
  cell,
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

- cell:

  The index of the grid cell, counting from zero.

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

The grid distribution function at the lower and upper cell edges, or two
infinities if the grid mass underflows to zero.
