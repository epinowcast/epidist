# The number of quadrature intervals each summary row is evaluated on

The moments and distribution function of a continuous estimand that is
truncated at the grid cutoff are computed by Simpson's rule on equally
spaced intervals running from `delay_min` to the cutoff, so the node
spacing is set by the cutoff and not by the scale of the delay. A fixed
number of intervals leaves a narrow delay unresolved on a wide grid,
which pins its implied kurtosis at its floor and can put its implied
standard deviation out by a factor of two. The number is therefore
chosen per study so that the spacing is at most a quarter of the spread
the study reported, see
[`.estimates_spread()`](https://epidist.epinowcast.org/reference/dot-estimates_spread.md),
with `options(epidist.meta_n_quad)` as its floor and
[`.meta_n_quad_max()`](https://epidist.epinowcast.org/reference/dot-meta_n_quad_max.md)
as its cap unless the option is set above it. It is even, because the
quadrature uses Simpson's rule.

## Usage

``` r
.estimates_n_quad(data)
```

## Arguments

- data:

  An `epidist_estimates_data` object.

## Value

An integer vector of interval counts, one per row.

## Details

Every row gets a number, including rows on the discrete grid, which do
not use it.
