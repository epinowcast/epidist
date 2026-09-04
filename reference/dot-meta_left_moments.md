# Summaries of a distribution left truncated at `lower`

A study that adjusted for right truncation and only counted delays above
`lower` reported the moments of the delay conditioned on exceeding it.
They are taken from the untruncated moments by removing the part below
`lower`, \\E\[\tau^k 1(\tau \le L)\] = L^k F(L) - \int_0^L k t^{k - 1}
F(t) dt\\, by Simpson's rule on \\\[0, L\]\\, and dividing by \\1 -
F(L)\\. The integral is over a bounded interval the quadrature resolves
well, so the result does not depend on `max_delay`, and it matches the
distribution function \\(F(y) - F(L)) / (1 - F(L))\\ used for the same
study's quantile rows. It applies to any distribution whose untruncated
moments and distribution function are available, which is the delay
itself and the uniform single interval approximation of
[`.meta_add_uniform()`](https://epidist.epinowcast.org/reference/dot-meta_add_uniform.md).
Matches `meta_family_left_moments` in
`inst/stan/meta_model/functions.stan`.

## Usage

``` r
.meta_left_moments(full, cdf, lower)
```

## Arguments

- full:

  A summary vector from
  [`.meta_moment_vector()`](https://epidist.epinowcast.org/reference/dot-meta_moment_vector.md)
  of the untruncated distribution.

- cdf:

  The distribution function at `n_quad + 1` equally spaced points
  running from zero to `lower`, for an even `n_quad`.

- lower:

  The study's minimum delay (its left truncation point).

## Value

A named numeric vector with elements `mean`, `sd`, `kurtosis` and
`skewness`.
