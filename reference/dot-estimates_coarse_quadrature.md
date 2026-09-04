# Studies whose quadrature nodes are far apart relative to their delays

The number of quadrature intervals of
[`.estimates_n_quad()`](https://epidist.epinowcast.org/reference/dot-estimates_n_quad.md)
is capped, so a study whose grid cutoff is very long relative to the
spread it reported is left with nodes further apart than a quarter of
that spread. This covers a study that did not adjust for right
truncation and used a continuous adjustment (`cens_adjusted` of 1, 2 or
4), a study that did adjust but whose primary events were not uniform
within their window (`cens_adjusted` of 2 or 4 with a non zero
`growth_rate`), and the quantile members of a covariance matrix group,
which are read off the same nodes.

## Usage

``` r
.estimates_coarse_quadrature(data)
```

## Arguments

- data:

  An `epidist_estimates_data` object.

## Value

A character vector of study identifiers whose quadrature is coarse.
