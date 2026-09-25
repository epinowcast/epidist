# Summarise draws of the delay parameters into a `<dist_spec>`

Summarise draws of the delay parameters into a `<dist_spec>`

## Usage

``` r
.dist_spec_from_draws(draws, family, max = Inf, cdf_max = 1)
```

## Arguments

- draws:

  A `data.frame` of draws of the `brms` parameters of the delay
  distribution for a single row of `newdata`, as one group of the result
  of
  [`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md).

- family:

  A list describing the family, as returned by
  [`.dist_spec_family()`](https://epidist.epinowcast.org/reference/dot-dist_spec_family.md).

- max:

  The maximum of the delay distribution, passed to
  [`distspec::bound_dist()`](https://epiforecasts.io/distspec/reference/bound_dist.html).
  Defaults to `Inf`, which is no maximum.

- cdf_max:

  The cumulative probability to keep the delay distribution up to,
  passed to
  [`distspec::bound_dist()`](https://epiforecasts.io/distspec/reference/bound_dist.html).
  Defaults to 1, which keeps the whole distribution.

## Value

A `<dist_spec>`.
