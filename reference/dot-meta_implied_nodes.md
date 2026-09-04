# The implied distribution function at equally spaced delays

Returns the implied distribution function on a grid of delays, together
with where that grid starts and how far apart its points are. Reading an
implied quantile off the delay scale needs the inverse of the implied
distribution function, which has no closed form on the discrete grid, so
it is interpolated from these points instead. See
[`.meta_node_quantile()`](https://epidist.epinowcast.org/reference/dot-meta_node_quantile.md).

## Usage

``` r
.meta_implied_nodes(dist, args, slots)
```

## Arguments

- dist:

  A `primarycensored` distribution function name.

- args:

  A named list of distribution parameters.

- slots:

  The output of
  [`.meta_row_slots()`](https://epidist.epinowcast.org/reference/dot-meta_row_slots.md).

## Value

A list with the distribution function `values`, the delay `origin` of
the first value, and the `spacing` between values.
