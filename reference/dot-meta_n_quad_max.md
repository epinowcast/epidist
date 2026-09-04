# The largest number of quadrature intervals chosen for a summary row

Every quadrature node costs a distribution function evaluation on each
gradient, so the number of intervals
[`.estimates_n_quad()`](https://epidist.epinowcast.org/reference/dot-estimates_n_quad.md)
chooses for a study is capped here unless
`options(epidist.meta_n_quad = )` is set higher.
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
warns about a study the cap leaves unresolved.

## Usage

``` r
.meta_n_quad_max()
```

## Value

An integer number of intervals.
