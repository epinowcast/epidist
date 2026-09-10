# Whether the implied quantile of a design is left on its chord

The chord inverse of
[`.meta_node_quantile()`](https://epidist.epinowcast.org/reference/dot-meta_node_quantile.md)
is refined only where the implied distribution function and density
exist in closed form. A discrete grid, an accrual estimand and a uniform
single interval estimand with a growing primary event are defined by the
interpolation between their nodes, so their chord is the implied
quantile and the slope of the interval holding it is the implied
density. See
[`.meta_refine_quantile()`](https://epidist.epinowcast.org/reference/dot-meta_refine_quantile.md).
Matches the early returns of `meta_family_node_quantile()` in Stan.

## Usage

``` r
.meta_quantile_on_chord(slots)
```

## Arguments

- slots:

  The output of
  [`.meta_row_slots()`](https://epidist.epinowcast.org/reference/dot-meta_row_slots.md).

## Value

A logical scalar.
