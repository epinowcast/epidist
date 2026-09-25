# Simulate delays from each draw of the distributional parameters

Simulation goes through the `brms` posterior prediction function for the
family, so it works for any family `brms` can predict from. Rows are
simulated in chunks to bound the memory used.

## Usage

``` r
.simulate_delays(family, dpars, nsim = 1000)
```

## Arguments

- family:

  A delay distribution family as returned by
  [`.resolve_delay_family()`](https://epidist.epinowcast.org/reference/dot-resolve_delay_family.md),
  a list with the family `name` and its distributional parameters
  `dpars`.

- dpars:

  A named list of distributional parameter vectors.

- nsim:

  The number of delays to simulate per row of `data`. Defaults to 1000.
  Only used when simulating.

## Value

A matrix with one row per element of the vectors in `dpars` and `nsim`
columns.
