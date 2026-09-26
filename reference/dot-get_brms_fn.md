# Get the post-processing function of a family by prefix

Gets the function `brms` would call for a family, such as `log_lik` or
`posterior_predict`. A family `epidist` defines itself, such as
[`gengamma()`](https://epidist.epinowcast.org/reference/gengamma.md),
carries these functions, so they are taken from it, whether it is given
as the family or as a model family built on it. For a `brms` family they
come from
[`.brms_family_fn()`](https://epidist.epinowcast.org/reference/dot-brms_family_fn.md).

## Usage

``` r
.get_brms_fn(prefix, family)
```

## Arguments

- prefix:

  Character string prefix of the brms function to get (e.g. "log_lik")

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

The requested function
