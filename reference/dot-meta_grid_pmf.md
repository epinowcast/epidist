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
to its window, so the follow up available to a delay of \\x\\ from the
start of that window is the accrual weight at \\w_p \lfloor x / w_p
\rfloor\\, a step function of \\x\\. Each cell is cut at the multiples
of `pwindow` inside it and every piece is weighted by the follow up at
the primary window it starts in. This is exact whenever `cutoff` is a
multiple of `pwindow`, and reduces to the weight at the cell's lower
edge when `pwindow` and `swindow` are equal.

A cohort grid is normalised by the distribution function at its top,
which is already known. An accrual grid reweights each cell first, so
its normaliser is not known in advance.
