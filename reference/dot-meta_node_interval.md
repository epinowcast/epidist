# The index of the node interval holding a delay

Interval `i` runs from node `i` to node `i + 1`. It is found by stepping
through the nodes rather than by rounding, because in Stan the delay is
a parameter and cannot be converted to an integer. Matches
`meta_family_node_interval()` in Stan.

## Usage

``` r
.meta_node_interval(nodes, q)
```

## Arguments

- nodes:

  The output of
  [`.meta_implied_nodes()`](https://epidist.epinowcast.org/reference/dot-meta_implied_nodes.md).

- q:

  A delay between the first and last node.

## Value

An integer between 1 and the number of intervals.
