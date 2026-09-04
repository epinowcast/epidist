# The implied summary and its standard error for one summary row and one draw

A standard error reported for a quantile row is on the scale of the
reported delay, as studies report it, so such a row is fitted on that
scale against the implied quantile of
[`.meta_node_quantile()`](https://epidist.epinowcast.org/reference/dot-meta_node_quantile.md).
A quantile row without a standard error is fitted on the cumulative
probability scale, where the binomial standard error of an empirical
distribution function applies.

## Usage

``` r
.meta_summary_terms(slots, dist, args, moments = NULL)
```

## Arguments

- slots:

  The output of
  [`.meta_row_slots()`](https://epidist.epinowcast.org/reference/dot-meta_row_slots.md).

- dist:

  A `primarycensored` distribution function name.

- args:

  A named list of distribution parameters for a single draw.

- moments:

  A summary vector from
  [`.meta_implied_moments()`](https://epidist.epinowcast.org/reference/dot-meta_implied_moments.md)
  for this row and draw, or `NULL` to compute it here.

## Value

A named numeric vector with elements `observed`, `implied` and `se`.

## Details

A group row stands for several summaries reported by one study, and this
returns the marginal of its first member, which is the reported mean of
a mean and standard deviation pair and the cumulative probability at the
smallest reported quantile of a quantile set. For a group covered by a
covariance matrix it is the first element of the reported vector, with
the first diagonal entry of the Cholesky factor as its standard error,
so a posterior predictive check of such a row describes that element
alone and not the rest of the group. That marginal is what the posterior
predictive draws for the row. The joint log likelihood of the whole
group is
[`.meta_row_log_lik()`](https://epidist.epinowcast.org/reference/dot-meta_row_log_lik.md).
