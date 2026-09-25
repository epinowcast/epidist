# Stan functions a family defines itself

A family `brms` does not have, such as
[`gengamma()`](https://epidist.epinowcast.org/reference/gengamma.md),
carries the Stan density and distribution function `brms` and the latent
model call. They are read from the `stan/family/` folder of the
installed package.

## Usage

``` r
.family_stanvars(family)
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

A `brms` `stanvars` object, or `NULL` for a `brms` family.
