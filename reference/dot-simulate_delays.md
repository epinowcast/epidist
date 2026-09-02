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

  A model fit with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md), a
  `brms` family, or the name of one, giving the delay distribution. If
  `NULL`, the default, the family is taken from `data`, which
  [`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)
  records on it. Some `dplyr` verbs drop that record, so pass the fit or
  the family if `data` has been through one of them.

- dpars:

  A named list of distributional parameter vectors.

- nsim:

  The number of delays to simulate per row of `data`. Defaults to 1000.
  Only used when simulating.

## Value

A matrix with one row per element of the vectors in `dpars` and `nsim`
columns.
