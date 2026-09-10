# The kernel of a forward pass step as a matrix

The binomial mass of a step from \\s\\ to \\t\\ counts splits into a
factor in \\s\\, a factor in \\t\\ and \\1 / (t - s)!\\, so the step is
a matrix in \\t - s\\ times a vector in \\s\\. The matrix holds
\\\exp((t - s) \lambda - \log (t - s)!)\\ scaled by
[`.meta_step_kernel_top()`](https://epidist.epinowcast.org/reference/dot-meta_step_kernel_top.md),
which keeps its entries in range around the expected step, and is zero
where \\t \< s\\. It depends on the parameters only through the scaling,
so Stan builds it as data and the step costs one matrix product on the
autodiff stack rather than an entry per pair of counts. Matches
`meta_family_step_kernel()` in Stan.

## Usage

``` r
.meta_step_kernel(a, b, a2, b2, lambda_scaled)
```

## Arguments

- a, b:

  The first and last source count.

- a2, b2:

  The first and last target count.

- lambda_scaled:

  The log expected step from
  [`.meta_fixed_point()`](https://epidist.epinowcast.org/reference/dot-meta_fixed_point.md).

## Value

A matrix with a row per target and a column per source count.
