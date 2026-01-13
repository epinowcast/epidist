# Reparameterise an `epidist` family to align `brms` and Stan

Reparameterise an `epidist` family to align `brms` and Stan

## Usage

``` r
epidist_family_param(family, ...)
```

## Arguments

- family:

  A description of the response distribution and link function to be
  used in the model. Every family function has a link argument allowing
  users to specify the link function to be applied on the response
  variable. If not specified, default links are used. For details of all
  supported families see `brmsfamily()`. Commonly used, such as
  [`lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html),
  are also reexported as part of `epidist`.

- ...:

  Additional arguments passed to `fn` method.

## See also

Other family:
[`epidist_family()`](https://epidist.epinowcast.org/dev/reference/epidist_family.md),
[`epidist_family_model()`](https://epidist.epinowcast.org/dev/reference/epidist_family_model.md),
[`epidist_family_model.default()`](https://epidist.epinowcast.org/dev/reference/epidist_family_model.default.md),
[`epidist_family_param.default()`](https://epidist.epinowcast.org/dev/reference/epidist_family_param.default.md)
