# The box constraints a set of integer day quantiles puts on a study

A type 1 quantile at probability \\p\\ of \\n\\ delays is their \\\lceil
n p \rceil\\th smallest, so two reported at probabilities that name the
same order statistic cannot land on different days. Such a group would
give every parameter a log likelihood of `-Inf`, so it is refused here
with a message that says why.

## Usage

``` r
.meta_crossing_box(estimates, study_n)
```

## Arguments

- estimates:

  The quantile rows of an `epidist_estimates_data` object making up one
  group, in increasing probability.

- study_n:

  The number of delays the quantiles were computed from.

## Value

A list of the `count` and `lower` member columns of
[`.meta_member_table()`](https://epidist.epinowcast.org/reference/dot-meta_member_table.md).
