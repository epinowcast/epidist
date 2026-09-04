# The smallest number of quadrature intervals used for a summary row

Set with `options(epidist.meta_n_quad = )`, as an even number of at
least two. Each summary row carries its own number of intervals in its
`n_quad` slot, chosen by
[`.estimates_n_quad()`](https://epidist.epinowcast.org/reference/dot-estimates_n_quad.md)
from the spread the study reported so that the quadrature resolves the
delay, and this is the floor of that choice. Set it before building the
model data, since the slot is filled in then. It also lifts the cap of
[`.meta_n_quad_max()`](https://epidist.epinowcast.org/reference/dot-meta_n_quad_max.md)
when set above it.

## Usage

``` r
.meta_n_quad()
```

## Value

An integer number of intervals.
