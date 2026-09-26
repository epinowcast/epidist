# Set the bin boundaries of the non-parametric family

Fixes the number of bins, and so the basis of the hazard formula, the
distributional parameters, their links and their bounds.

## Usage

``` r
.np_set_boundaries(family, boundaries)
```

## Arguments

- family:

  A family built by
  [`nonparametric()`](https://epidist.epinowcast.org/reference/nonparametric.md).

- boundaries:

  A numeric vector of at least four strictly increasing bin boundaries,
  \\b_0\\ to \\b_K\\. The default, `NULL`, is set by
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md)
  from the data, with a bin for every whole delay from 0 up to the
  longest delay in the data, and at least four bins, that is
  `seq(-1, max(3, max_delay))`. The last boundary must be at least as
  long as the longest observed delay.

## Value

The family with its boundaries, basis and parameters set.
