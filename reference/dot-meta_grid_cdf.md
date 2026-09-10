# The uncorrected grid distribution function at a set of cells

The number of delays a study saw at or below a grid cell is binomial on
the grid distribution function at that cell, which is the mass of the
cells up to and including it. A cohort grid is normalised by the mass it
holds, so only the cells asked for, the top of the grid and its first
cell are evaluated, as in
[`.meta_grid_edges()`](https://epidist.epinowcast.org/reference/dot-meta_grid_edges.md).
An accrual grid reweights every cell before renormalising, so it is
built in full by
[`.meta_grid_pmf()`](https://epidist.epinowcast.org/reference/dot-meta_grid_pmf.md).

## Usage

``` r
.meta_grid_cdf(
  cell,
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

- cell:

  An integer vector of grid cell indices, counting from zero.

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

The grid distribution function at each cell, or infinities if the grid
mass underflows to zero.
