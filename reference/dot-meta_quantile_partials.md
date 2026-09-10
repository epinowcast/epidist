# The centred partial moments of an estimand below its implied quantile

The sampling covariance of a reported mean or standard deviation with a
reported quantile depends on \\\int_L^{q} (x - \mu)^k \text{d}G(x)\\ for
\\k = 1, 2\\, with \\G\\ the implied distribution function, \\\mu\\ the
implied mean and \\L\\ the smallest delay the study counted. Integrating
by parts gives \\(q - \mu)^k p - \int_L^{q} k (x - \mu)^{k - 1} G(x)
\text{d}x\\, and the remaining integral is taken by the trapezoid rule
over the linear interpolant of the nodes, which reaches `p` at `q`. Its
error is of the order of the node spacing squared, which
[`.estimates_n_quad()`](https://epidist.epinowcast.org/reference/dot-estimates_n_quad.md)
holds to a quarter of the reported spread. Matches
`meta_family_quantile_partials()` in Stan.

## Usage

``` r
.meta_quantile_partials(nodes, q, p, index, centre)
```

## Arguments

- nodes:

  The output of
  [`.meta_implied_nodes()`](https://epidist.epinowcast.org/reference/dot-meta_implied_nodes.md).

- q:

  The implied quantile from
  [`.meta_node_quantile()`](https://epidist.epinowcast.org/reference/dot-meta_node_quantile.md).

- p:

  The probability of the quantile.

- index:

  The node interval holding `q`, from
  [`.meta_node_interval()`](https://epidist.epinowcast.org/reference/dot-meta_node_interval.md).

- centre:

  The implied mean of the estimand.

## Value

A numeric vector of the first and second centred partial moments.
