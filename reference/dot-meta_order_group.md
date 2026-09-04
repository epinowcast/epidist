# Order the summaries within a joint likelihood group

A mean and standard deviation pair is stored with the mean first so that
the bivariate normal knows which member is which. A set of quantiles is
stored in increasing probability, which must also be non decreasing in
the reported value for the cells of the multinomial to be a partition of
the delay axis. Coincident values are merged into one cell by the
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
