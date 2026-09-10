# Evaluate the density of the delay distribution at each draw

Uses the density of the family when there is an analytic solution, and
otherwise simulates delays from each draw with
[`.simulate_delays()`](https://epidist.epinowcast.org/reference/dot-simulate_delays.md)
and estimates their density with
[`stats::density()`](https://rdrr.io/r/stats/density.html).

## Usage

``` r
.delay_density_grid(family, dpars, max_delay = NULL, n_grid = 101, nsim = 1000)
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

- max_delay:

  The largest delay to evaluate the delay distribution at when
  `type = "delay"`. If `NULL`, the default, the posterior median of the
  99% quantile of the delay distribution is used.

- n_grid:

  The number of delays to evaluate the density at.

- nsim:

  The number of delays to simulate per row of `data`. Defaults to 1000.
  Only used when simulating.

## Value

A list with `delays`, the delays the density is evaluated at, and
`density`, a matrix with one row per draw and one column per delay.
