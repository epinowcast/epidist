# The density of a continuous estimand at its implied quantile

Where the quantile is left on its chord, see
[`.meta_quantile_on_chord()`](https://epidist.epinowcast.org/reference/dot-meta_quantile_on_chord.md),
the estimand is the linear interpolant of its nodes and its density is
the slope of the interval holding the quantile. Otherwise it is the
closed form density of
[`.meta_implied_density()`](https://epidist.epinowcast.org/reference/dot-meta_implied_density.md).
Matches `meta_family_quantile_density()` in Stan.

## Usage

``` r
.meta_quantile_density(q, index, nodes, dist, args, slots)
```

## Arguments

- q:

  The implied quantile from
  [`.meta_node_quantile()`](https://epidist.epinowcast.org/reference/dot-meta_node_quantile.md).

- index:

  The node interval holding `q`, from
  [`.meta_node_interval()`](https://epidist.epinowcast.org/reference/dot-meta_node_interval.md).

- nodes:

  The output of
  [`.meta_implied_nodes()`](https://epidist.epinowcast.org/reference/dot-meta_implied_nodes.md).

- dist:

  A `primarycensored` distribution function name, or `NULL` to return
  the chord inverse alone.

- args:

  A named list of distribution parameters.

- slots:

  The output of
  [`.meta_row_slots()`](https://epidist.epinowcast.org/reference/dot-meta_row_slots.md).

## Value

A density on the delay scale.
