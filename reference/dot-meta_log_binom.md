# The log mass of a binomial count with floored probabilities

The binomial mass with the success and failure probabilities floored at
1e-300, so that a count the parameters make all but impossible is very
unlikely rather than impossible and a chain can start from a random
initial value. Matches the arithmetic of `meta_family_grid_box_ll()` in
Stan.

## Usage

``` r
.meta_log_binom(x, size, prob)
```

## Arguments

- x:

  A vector of counts.

- size:

  A vector of numbers of trials.

- prob:

  The success probability.

## Value

A vector of log masses.
