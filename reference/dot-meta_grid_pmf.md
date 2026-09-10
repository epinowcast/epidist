# The discrete delay distribution a naive study would observe

Builds the probability mass function of the interval censored delays a
study that took date differences directly would have summarised. The
grid runs over delays of `0`, `swindow`, `2 * swindow`, and so on up to
the largest multiple of `swindow` whose upper bound is within `cutoff`,
and is renormalised so that it conditions on delays falling within the
grid. This renormalisation is what applies the study's right truncation,
and it discretises the truncation point to the nearest grid boundary.

## Usage

``` r
.meta_grid_pmf(
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

A numeric vector of probabilities summing to one, one per kept cell, or
a vector of `NA` the same length if the grid mass underflows to zero.

## Details

Cells recording a delay below `lower` are dropped before the grid is
renormalised, which conditions it on the study's left truncation point.
The normaliser is then the mass of the kept cells, which is \\F(D) -
F(L)\\ whenever `lower` falls on a grid boundary.

Under an accrual design the cell masses are additionally weighted by the
follow up available to the cases each cell holds, before renormalising.
A case is seen when its primary event fell early enough for its delay to
complete before the calendar stop, and the primary event is known only
to its window. A complete primary window starting at \\k w_p\\ holds a
delay of \\x\\ from its start when \\k w_p + x \le A\\, so the growth
weighted mass of the complete windows eligible for \\x\\ is a step
function of \\x\\ that steps down at \\A - j w_p\\. Each cell is cut at
those points and every piece is weighted by that mass. When `cutoff` is
not a multiple of `pwindow` the last primary window is partial, of
length \\l = A - w_p \lfloor A / w_p \rfloor\\. It only holds delays up
to \\l\\, and the offset of its primary events runs over \\l\\ rather
than \\w_p\\, so its cases follow the primary censored distribution
function with a window of \\l\\, weighted by the growth weighted length
of the window, and are added to the cells below \\l\\. This is exact for
any `cutoff`, `pwindow` and `swindow`, and reduces to the weight at the
cell's lower edge when `pwindow` and `swindow` are equal and `cutoff` is
a multiple of both.

A cohort grid is normalised by the distribution function at its top,
which is already known. An accrual grid reweights each cell first, so
its normaliser is not known in advance.
