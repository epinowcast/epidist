# Stan parameterisation of the non-parametric family

The delay distribution parameters are the boundaries followed by the bin
hazards, built from `mu`, the basis and the coefficients by the Stan
function `epidist_np_params()`. The boundaries and the basis are written
into the Stan code as functions returning them, see
[`.np_stanvars()`](https://epidist.epinowcast.org/reference/dot-np_stanvars.md).

## Usage

``` r
# S3 method for class 'nonparametric'
epidist_family_param(family, ...)
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

- ...:

  Additional arguments passed to `fn` method.

## Value

The family with a `param` element holding the Stan expression for the
parameter array.

## See also

Other family:
[`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md),
[`epidist_family_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.md),
[`epidist_family_model.default()`](https://epidist.epinowcast.org/reference/epidist_family_model.default.md),
[`epidist_family_param()`](https://epidist.epinowcast.org/reference/epidist_family_param.md),
[`epidist_family_param.default()`](https://epidist.epinowcast.org/reference/epidist_family_param.default.md),
[`epidist_family_param.gengamma()`](https://epidist.epinowcast.org/reference/epidist_family_param.gengamma.md),
[`gengamma()`](https://epidist.epinowcast.org/reference/gengamma.md),
[`nonparametric()`](https://epidist.epinowcast.org/reference/nonparametric.md)
