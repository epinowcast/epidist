# Resolve the delay distribution family of a `data.frame` of draws

Resolve the delay distribution family of a `data.frame` of draws

## Usage

``` r
.resolve_delay_family(data, family = NULL)
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

## Value

A list with the delay distribution `name` and its distributional
parameters `dpars`.
