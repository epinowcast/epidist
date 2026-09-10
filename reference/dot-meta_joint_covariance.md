# The sampling covariance of the summaries one study reports

The asymptotic covariance of a sample mean, a sample standard deviation
and sample quantiles computed from the same `study_n` delays. The mean
and standard deviation block is that of
[`.meta_moment_pair_ll()`](https://epidist.epinowcast.org/reference/dot-meta_moment_pair_ll.md).
By the Bahadur representation a sample quantile at probability \\p\\ is
\\Q_p - (\hat{G}(Q_p) - p) / f(Q_p)\\ up to a smaller order term, so
with \\f_i = f(Q\_{p_i})\\ the implied density at each implied quantile
\$\$\text{Cov}(q_i, q_j) = \frac{p_i (1 - p_j)}{n f_i f_j}, \quad p_i
\le p_j,\$\$ \$\$\text{Cov}(\bar{x}, q_i) = -\frac{1}{n f_i}
\int_L^{Q\_{p_i}} (x - \mu) \text{d}G(x),\$\$ \$\$\text{Cov}(s, q_i) =
-\frac{1}{2 \sigma n f_i} \int_L^{Q\_{p_i}} \left((x - \mu)^2 -
\sigma^2\right) \text{d}G(x),\$\$ the last carried from the sample
variance to the standard deviation by the delta method. The integrals
are the centred partial moments of
[`.meta_quantile_partials()`](https://epidist.epinowcast.org/reference/dot-meta_quantile_partials.md).
Matches `meta_family_joint_covariance()` in Stan.

## Usage

``` r
.meta_joint_covariance(types, probs, moments, density, partial, study_n)
```

## Arguments

- types:

  Member types, 1 for a mean, 2 for a standard deviation and 3 for a
  quantile.

- probs:

  Member probabilities, zero for a mean or standard deviation.

- moments:

  A summary vector from
  [`.meta_moment_vector()`](https://epidist.epinowcast.org/reference/dot-meta_moment_vector.md).

- density:

  The implied density at each implied quantile, one per quantile member
  in order.

- partial:

  A two row matrix of centred partial moments from
  [`.meta_quantile_partials()`](https://epidist.epinowcast.org/reference/dot-meta_quantile_partials.md),
  one column per quantile member in order.

- study_n:

  The number of delays the summaries were computed from.

## Value

A covariance matrix over the members.
