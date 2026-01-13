# Define `epidist` family

This function is used within
[`epidist()`](https://epidist.epinowcast.org/dev/reference/epidist.md)
to create a model specific custom `brms` family object. This custom
family is passed to `brms`. It is unlikely that as a user you will need
this function, but we export it nonetheless to be transparent about what
happens inside of a call to
[`epidist()`](https://epidist.epinowcast.org/dev/reference/epidist.md).

## Usage

``` r
epidist_family(data, family = lognormal(), ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

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
[`epidist_family_model()`](https://epidist.epinowcast.org/dev/reference/epidist_family_model.md),
[`epidist_family_model.default()`](https://epidist.epinowcast.org/dev/reference/epidist_family_model.default.md),
[`epidist_family_param()`](https://epidist.epinowcast.org/dev/reference/epidist_family_param.md),
[`epidist_family_param.default()`](https://epidist.epinowcast.org/dev/reference/epidist_family_param.default.md)
