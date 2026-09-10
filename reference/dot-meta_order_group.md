# Order the summaries within a joint likelihood group

Members are stored as the mean, then the standard deviation, then the
quantiles in increasing probability, so that the bivariate normal knows
which member is which and a joint study group is laid out as its
covariance is built. The quantiles must be non decreasing in the
reported value for the cells of the multinomial to be a partition of the
delay axis. Coincident values are merged into one cell by the
likelihood.

## Usage

``` r
.meta_order_group(estimates, vcov = NULL)
```

## Arguments

- estimates:

  The rows of an `epidist_estimates_data` object making up one group.

- vcov:

  The covariance matrix over the group's summaries, or `NULL`.

## Value

The input, reordered.

## Details

A group covered by a covariance matrix keeps the order its rows were
given in, because that is the order the matrix is indexed by.
