# Simulate delays from each draw of the non-parametric family

Simulate delays from each draw of the non-parametric family

## Usage

``` r
.np_simulate_delays(np, dpars, nsim = 1000)
```

## Arguments

- np:

  The `np` element of the family, holding the boundaries, the basis and
  its coefficients.

- dpars:

  A named list of distributional parameter vectors.

- nsim:

  The number of delays to simulate per row of `data`. Defaults to 1000.
  Only used when simulating.

## Value

A matrix with one row per element of the vectors in `dpars` and `nsim`
columns.
