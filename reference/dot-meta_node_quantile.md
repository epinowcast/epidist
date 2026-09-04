# The delay at which an implied distribution function reaches `p`

Inverts the distribution function of
[`.meta_implied_nodes()`](https://epidist.epinowcast.org/reference/dot-meta_implied_nodes.md)
by linear interpolation between the two points that bracket `p`. On the
discrete grid that interpolant is the model's own definition of the
continuity corrected quantile, so the chord is exact there. For a
continuous estimand the chord is only as accurate as the node spacing,
which for a truncation adjusted study is about a day, so it is refined
by
[`.meta_refine_quantile()`](https://epidist.epinowcast.org/reference/dot-meta_refine_quantile.md)
when the design is supplied. The result stays a differentiable function
of the delay distribution parameters, which a root search would not be.

## Usage

``` r
.meta_node_quantile(nodes, p, dist = NULL, args = NULL, slots = NULL)
```

## Arguments

- nodes:

  The output of
  [`.meta_implied_nodes()`](https://epidist.epinowcast.org/reference/dot-meta_implied_nodes.md).

- p:

  A probability.

- dist:

  A `primarycensored` distribution function name, or `NULL` to return
  the chord inverse alone.

- args:

  A named list of distribution parameters.

- slots:

  The output of
  [`.meta_row_slots()`](https://epidist.epinowcast.org/reference/dot-meta_row_slots.md).

## Value

A delay.
