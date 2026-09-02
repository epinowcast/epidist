# Add summaries by simulating from the delay distribution

Add summaries by simulating from the delay distribution

## Usage

``` r
.sample_summaries(data, family, probs = NULL, nsim = 1000)
```

## Arguments

- data:

  A `data.frame` of draws of the distributional parameters, as returned
  by
  [`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md).

- family:

  A model fit with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md), a
  `brms` family, or the name of one, giving the delay distribution. If
  `NULL`, the default, the family is taken from `data`, which
  [`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)
  records on it. Some `dplyr` verbs drop that record, so pass the fit or
  the family if `data` has been through one of them.

- probs:

  A numeric vector of probabilities to add quantiles of the delay
  distribution for. If `NULL`, the default, no quantiles are added.

- nsim:

  The number of delays to simulate per row of `data`. Defaults to 1000.
  Only used when simulating.

## Value

The input with summary columns added.
