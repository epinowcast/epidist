# Analytic summaries of a delay distribution

The mean and standard deviation mirror the formulas used by
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md).
A draw wide enough to overflow a moment, such as a lognormal whose
`exp(4 * sdlog^2)` is infinite, is returned as
[`.meta_moment_failure()`](https://epidist.epinowcast.org/reference/dot-meta_moment_failure.md)
so that the row is rejected rather than carrying a `NaN` into its
standard error. Matches the reject in `meta_family_moments()` in
`inst/stan/meta_model/functions.stan`, where an infinite intermediate
would otherwise leave a finite density with a non finite gradient.

## Usage

``` r
.meta_continuous_moments(dist, args)
```

## Arguments

- dist:

  A `primarycensored` distribution function name.

- args:

  A named list of distribution parameters.

## Value

A named numeric vector with elements `mean`, `sd`, `kurtosis` and
`skewness`.
