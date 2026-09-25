# Build the analytic delay summaries of a family

Build the analytic delay summaries of a family

## Usage

``` r
.analytic_family(dpars, dist, natural, mean, sd)
```

## Arguments

- dpars:

  The `brms` parameters of the family.

- dist:

  The distribution function name, as used by
  [`.pdist()`](https://epidist.epinowcast.org/reference/dot-dist_fn.md).

- natural:

  A function of the `brms` parameters returning the named arguments of
  the distribution's R functions.

- mean, sd:

  Functions of the `brms` parameters returning the mean and the standard
  deviation.

## Value

A list with `dpars` and the functions `mean(d)`, `sd(d)`,
`quantile(d, p)` and `density(d, x)` of the `brms` parameters `d`.
