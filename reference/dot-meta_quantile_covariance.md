# The entries of the joint sampling covariance of one study's summaries

`.meta_quantile_covariance()` gives the Bahadur covariance of two
quantiles, `.meta_cross_covariance()` that of a mean or a standard
deviation and a quantile, and `.meta_moment_covariance()` that of two
moment summaries, see
[`.meta_joint_covariance()`](https://epidist.epinowcast.org/reference/dot-meta_joint_covariance.md).
They mirror the Stan functions `meta_family_quantile_covariance()`,
`meta_family_cross_covariance()` and `meta_family_moment_covariance()`
in `inst/stan/meta_model/functions.stan`.

## Usage

``` r
.meta_quantile_covariance(prob_i, prob_j, density_i, density_j, study_n)

.meta_cross_covariance(moment_type, prob, density, partial, spread, study_n)

.meta_moment_covariance(type_i, type_j, se_mean, se_sd, rho)
```

## Arguments

- prob_i, prob_j, prob:

  Member probabilities.

- density_i, density_j, density:

  The implied density at the implied quantiles.

- study_n:

  The number of delays the summaries were computed from.

- moment_type, type_i, type_j:

  Member types, 1 for a mean and 2 for a standard deviation.

- partial:

  The two centred partial moments at the quantile, a column of
  [`.meta_quantile_partials()`](https://epidist.epinowcast.org/reference/dot-meta_quantile_partials.md).

- spread:

  The implied standard deviation.

- se_mean, se_sd:

  The sampling standard errors of the mean and the standard deviation.

- rho:

  The sampling correlation of the mean and the standard deviation from
  [`.meta_moment_correlation()`](https://epidist.epinowcast.org/reference/dot-meta_moment_correlation.md).

## Value

A covariance.
