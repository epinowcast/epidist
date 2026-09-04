# The cumulative counts implied by a set of reported quantiles

The multinomial likelihood of
[`.meta_quantile_set_ll()`](https://epidist.epinowcast.org/reference/dot-meta_quantile_set_ll.md)
needs the number of delays a study saw at or below each reported
quantile. Rounding the cumulative probabilities rather than the
increments keeps the counts non decreasing and bounded by the sample
size, so the cell counts are non negative and sum to the sample size
however the probabilities round.

## Usage

``` r
.meta_quantile_counts(p, study_n)
```

## Arguments

- p:

  A vector of quantile probabilities in increasing order.

- study_n:

  The number of delays the quantiles were computed from.

## Value

An integer vector of cumulative counts.
