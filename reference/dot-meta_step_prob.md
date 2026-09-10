# The probability of a step of the chain of cumulative counts

Given the count at one edge, the delays beyond it fall at or below the
next edge with this probability. A distribution function that has
reached one leaves nothing to place, so the step probability is zero.

## Usage

``` r
.meta_step_prob(from, to)
```

## Arguments

- from, to:

  The grid distribution function at the two edges.

## Value

A probability.
