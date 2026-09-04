# Summarise a distribution from its mean and central moments

The kurtosis is needed as well as the mean and standard deviation
because the sampling error of a reported standard deviation depends on
it. See
[`.meta_summary_terms()`](https://epidist.epinowcast.org/reference/dot-meta_summary_terms.md).
The skewness is needed because the sampling covariance of a reported
mean and a reported standard deviation from the same study depends on
it. See
[`.meta_moment_pair_ll()`](https://epidist.epinowcast.org/reference/dot-meta_moment_pair_ll.md).

## Usage

``` r
.meta_moment_vector(mean, variance, third, fourth)
```

## Arguments

- mean:

  The mean of the distribution.

- variance:

  The variance of the distribution.

- third:

  The third central moment of the distribution.

- fourth:

  The fourth central moment of the distribution.

## Value

A named numeric vector with elements `mean`, `sd`, `kurtosis` and
`skewness`.
