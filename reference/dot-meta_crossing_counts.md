# The order statistics a set of type 1 quantiles stand for

A type 1 quantile at probability \\p\\ of \\n\\ delays is the \\\lceil n
p \rceil\\th smallest of them. A quantile of integer day delays reported
as day \\y\\ therefore says the study saw fewer than that many delays
below \\y\\ and at least that many at or below it, which are the box
constraints
[`.meta_grid_box_ll()`](https://epidist.epinowcast.org/reference/dot-meta_grid_box_ll.md)
fits. The rounding guard matches
[`.meta_grid_crossing_ll()`](https://epidist.epinowcast.org/reference/dot-meta_grid_crossing_ll.md),
so that \\n p\\ landing on an integer up to floating point error is not
pushed up a count.

## Usage

``` r
.meta_crossing_counts(p, study_n)
```

## Arguments

- p:

  A vector of quantile probabilities in increasing order.

- study_n:

  The number of delays the quantiles were computed from.

## Value

An integer vector of counts.
