# Family specific prior distributions

This function contains `brms` prior distributions which are specific to
particular likelihood families e.g.
[`brms::lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html).

## Usage

``` r
epidist_family_prior(family, ...)
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

Other prior:
[`epidist_family_prior.default()`](https://epidist.epinowcast.org/reference/epidist_family_prior.default.md),
[`epidist_family_prior.lognormal()`](https://epidist.epinowcast.org/reference/epidist_family_prior.lognormal.md),
[`epidist_model_prior()`](https://epidist.epinowcast.org/reference/epidist_model_prior.md),
[`epidist_model_prior.default()`](https://epidist.epinowcast.org/reference/epidist_model_prior.default.md),
[`epidist_prior()`](https://epidist.epinowcast.org/reference/epidist_prior.md)
