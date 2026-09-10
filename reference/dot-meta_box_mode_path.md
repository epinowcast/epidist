# The most likely path of the constrained chain of cumulative counts

A Viterbi pass over the coarse grid of
[`.meta_box_grid()`](https://epidist.epinowcast.org/reference/dot-meta_box_grid.md)
at every edge, which finds the counts a study most probably had at each
edge given every box, to within a stride. The forward pass of
[`.meta_grid_box_ll()`](https://epidist.epinowcast.org/reference/dot-meta_grid_box_ll.md)
is then kept to a band around them. Centring on the counts the boxes
pull the chain to, rather than on the counts the parameters expect at
each edge, is what keeps the pass accurate where a later quantile forces
an earlier count far from its mean. The distribution function is rounded
to \\2^{-20}\\ by
[`.meta_fixed_point()`](https://epidist.epinowcast.org/reference/dot-meta_fixed_point.md)
first, as Stan must to keep the search off the autodiff stack. Matches
`meta_family_box_mode_path()` in Stan.

## Usage

``` r
.meta_box_mode_path(study_n, cdf, lower, upper, stride)
```

## Arguments

- study_n:

  The number of delays the quantiles were computed from.

- cdf:

  The grid distribution function at the edges.

- lower, upper:

  The boxes at the edges.

- stride:

  The stride of the grid from
  [`.meta_box_stride()`](https://epidist.epinowcast.org/reference/dot-meta_box_stride.md).

## Value

An integer vector of counts, one per edge, or `NULL` when no path
satisfies every box.
