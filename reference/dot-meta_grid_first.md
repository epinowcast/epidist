# The index of the first grid cell a left truncated study could have seen

A study that only counted delays of at least `lower` never saw the grid
cells recording a shorter delay, so they are dropped before the grid is
renormalised. The index counts cells from zero, so it is zero when the
study counted every delay.

## Usage

``` r
.meta_grid_first(lower, swindow)
```

## Arguments

- lower:

  The study's minimum delay (its left truncation point).

- swindow:

  The secondary censoring window width, which is also the grid spacing.

## Value

An integer cell index.
