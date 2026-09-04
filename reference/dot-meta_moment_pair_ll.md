# The joint log likelihood of a mean and a standard deviation from one study

A sample mean and a sample standard deviation computed from the same
delays are correlated, so fitting them as two independent normal terms
overstates how much a study reporting both tells us. They are instead
given the asymptotic bivariate normal of the pair, with
\\\text{Cov}(\bar{x}, s^2) = \mu_3 / n\\ carried onto the standard
deviation scale by the delta method, giving \\\text{Cov}(\bar{x}, s) =
\mu_3 / (2 n \sigma)\\ and a correlation of \\\gamma_1 / \sqrt{\kappa -
1}\\. See
[`.meta_moment_correlation()`](https://epidist.epinowcast.org/reference/dot-meta_moment_correlation.md).

## Usage

``` r
.meta_moment_pair_ll(y_mean, y_sd, moments, study_n)
```

## Arguments

- y_mean:

  The reported mean.

- y_sd:

  The reported standard deviation.

- moments:

  A summary vector from
  [`.meta_moment_vector()`](https://epidist.epinowcast.org/reference/dot-meta_moment_vector.md).

- study_n:

  The number of delays the summaries were computed from.

## Value

A log density.
