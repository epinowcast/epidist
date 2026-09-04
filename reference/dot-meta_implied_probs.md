# The cumulative probabilities a study would report at several quantiles

The vectorised form of
[`.meta_implied_prob()`](https://epidist.epinowcast.org/reference/dot-meta_implied_prob.md),
used for the set of quantiles one study reported. A cohort study on a
discrete grid needs only the cell edges the reported values fall
between, so all of them are evaluated in one call. Every other design
falls back to evaluating each value on its own.

## Usage

``` r
.meta_implied_probs(y, dist, args, slots)
```

## Arguments

- y:

  A vector of reported quantile values.

- dist:

  A `primarycensored` distribution function name.

- args:

  A named list of distribution parameters.

- slots:

  The output of
  [`.meta_row_slots()`](https://epidist.epinowcast.org/reference/dot-meta_row_slots.md).

## Value

A numeric vector of probabilities.
