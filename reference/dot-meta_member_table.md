# Build the member table of one joint likelihood group

The member type and probability are carried alongside the reported value
because a group covered by a covariance matrix may mix means, standard
deviations and quantiles, so the likelihood needs to know which implied
summary each member is.

## Usage

``` r
.meta_member_table(estimates, count)
```

## Arguments

- estimates:

  The rows of an `epidist_estimates_data` object making up one group,
  already ordered.

- count:

  The cumulative counts the multinomial quantile likelihood uses, or
  zeros for a group that does not use it.

## Value

A tibble of member `value`, `count`, `type` and `p` columns.
