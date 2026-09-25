# The name of a delay distribution family

A family built with
[`brms::custom_family()`](https://paulbuerkner.com/brms/reference/custom_family.html),
such as
[`gengamma()`](https://epidist.epinowcast.org/reference/gengamma.md), is
named `"custom"` for `brms` to dispatch on and records its own name in
`name`. Every other family is named by `family`.

## Usage

``` r
.family_name(family)
```

## Arguments

- family:

  A description of the response distribution and link function to be
  used in the model. Every family function has a link argument allowing
  users to specify the link function to be applied on the response
  variable. If not specified, default links are used. For details of all
  supported families see
  [`brms::brmsfamily()`](https://paulbuerkner.com/brms/reference/brmsfamily.html).
  Commonly used, such as
  [`brms::lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  are also reexported as part of `epidist`.

## Value

A character string.
