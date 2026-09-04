# The log likelihood of one meta model summary row for one draw

Ungrouped rows use the normal approximations of
[`.meta_summary_terms()`](https://epidist.epinowcast.org/reference/dot-meta_summary_terms.md).
A group row, which stands for several summaries reported by one study,
uses the joint likelihood of its members:
[`.meta_moment_pair_ll()`](https://epidist.epinowcast.org/reference/dot-meta_moment_pair_ll.md)
for a mean and a standard deviation, and
[`.meta_quantile_set_ll()`](https://epidist.epinowcast.org/reference/dot-meta_quantile_set_ll.md)
for a set of quantiles.

## Usage

``` r
.meta_row_log_lik(slots, dist, args, moments = NULL)
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

A log density.

## Details

A draw whose implied moments are not all finite, which an extreme delay
distribution parameter can produce by overflowing the analytic kurtosis,
is rejected with a log likelihood of `-Inf` rather than `NaN`, for every
row that uses the moments. Matches the guard in `meta_family_lpmf` in
`inst/stan/meta_model/functions.stan`.
