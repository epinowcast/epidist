# Take a quantile of the proportions of every draw

A draw with no prediction in a bin has no row for it, and so a
proportion of zero. Pads the proportions back up to one per draw before
taking the quantile.

## Usage

``` r
.zero_padded_quantile(p, n_draws, prob)
```

## Arguments

- p:

  The proportions of the draws that predicted into the bin.

- n_draws:

  The number of draws.

- prob:

  The probability of the quantile to take.

## Value

A number.
