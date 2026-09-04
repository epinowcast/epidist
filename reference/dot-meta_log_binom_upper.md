# The log upper tail of a binomial count, stable far into the tail

`P(M >= m)` for `M ~ Binomial(size, prob)`. Nine standard deviations
above the mean, or below a probability of 1e-12, the tail is summed term
by term on the log scale until the terms fall forty nats below the
first, or two hundred terms in with a geometric bound on the rest, which
is exact to that tolerance and, unlike the distribution function, has
finite partial derivatives there in Stan. Elsewhere it is the
distribution function of the complement. Matches
`meta_family_log_binom_upper()` in Stan.

## Usage

``` r
.meta_log_binom_upper(m, size, prob)
```

## Arguments

- m:

  The smallest count in the tail, a vector.

- size:

  The number of trials, a vector.

- prob:

  The success probability, a vector.

## Value

The log tail probabilities, recycled to the longest argument.
