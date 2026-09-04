# Summaries implied by a distribution function evaluated on a grid

Uses Simpson's rule on the survival integrals \\\int_L^D k t^{k - 1}
(F(D) - F(t)) dt\\, which with the boundary term \\L^k (F(D) - F(L))\\
give the first four raw moments of the distribution truncated to \\(L,
D\]\\. The boundary term vanishes when \\L\\ is zero, recovering the
untruncated expression. Matches the implementation in
`inst/stan/meta_model/functions.stan`.

## Usage

``` r
.meta_survival_moments(cdf, lower = 0, cutoff)
```

## Arguments

- cdf:

  The distribution function at `n_quad + 1` equally spaced points
  running from `lower` to `cutoff`, for an even `n_quad`.

- lower:

  The study's minimum delay (its left truncation point).

- cutoff:

  The right truncation point.

## Value

A named numeric vector with elements `mean`, `sd` and `kurtosis`, or
[`.meta_moment_failure()`](https://epidist.epinowcast.org/reference/dot-meta_moment_failure.md)
if the distribution function holds no mass between `lower` and `cutoff`.
