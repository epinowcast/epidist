# Simulate secondary events based on a delay distribution

This function simulates secondary events based on a given delay
distribution. The input dataset should have the primary event times in a
column named `ptime`.

## Usage

``` r
simulate_secondary(data, dist = rlnorm, ...)
```

## Arguments

- data:

  A data frame with the primary event times.

- dist:

  The delay distribution to be used. Defaults to
  [`rlnorm()`](https://rdrr.io/r/stats/Lognormal.html).

- ...:

  Arguments to be passed to the delay distribution function.

## Value

A `data.frame` that augments `data` with two new columns: `delay`
(secondary event latency) and `stime` (the time of the secondary event).

## See also

Other simulate:
[`simulate_exponential_cases()`](https://epidist.epinowcast.org/dev/reference/simulate_exponential_cases.md),
[`simulate_gillespie()`](https://epidist.epinowcast.org/dev/reference/simulate_gillespie.md),
[`simulate_uniform_cases()`](https://epidist.epinowcast.org/dev/reference/simulate_uniform_cases.md)
