# The bins the hazard formula is evaluated on

The bins the hazard formula is evaluated on

## Usage

``` r
.np_bins(boundaries)
```

## Arguments

- boundaries:

  A numeric vector of at least four strictly increasing bin boundaries,
  \\b_0\\ to \\b_K\\. The default, `NULL`, is set by
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md)
  from the data, with a bin for every whole delay from 0 up to the
  longest delay in the data, and at least four bins, that is
  `seq(-1, max(3, max_delay))`. The last boundary must be at least as
  long as the longest observed delay.

## Value

A `data.frame` with one row per bin whose hazard is free, all but the
last, holding the right edge of the bin as `delay` and its index as the
factor `bin`.
