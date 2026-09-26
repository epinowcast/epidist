# Family specific prior distributions for the non-parametric family

The intercept of `mu`, the mean logit hazard, gets a normal prior with a
standard deviation of 1.5 centred on \\\mathrm{logit}(1 / K)\\, the
hazard of the first of \\K\\ bins when every bin is equally likely. A
prior centred on a hazard of a half would put half the prior mass on the
first bin, so the prior mean delay would be about a day whatever the
bins. Each unpenalised coefficient `h<i>b` gets `normal(0, 2)`, on the
logit scale since the basis is scaled. The intercept of each standard
deviation `h<j>sd` gets `normal(0, 1)` on the log scale, the prior
`primarycensored` uses for the spread of the logit hazards. Each
standardised coefficient `h<i>z` gets `std_normal()`, which makes the
penalised terms non-centred.

## Usage

``` r
# S3 method for class 'nonparametric'
epidist_family_prior(family, formula, ...)
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

- formula:

  An object of class
  [stats::formula](https://rdrr.io/r/stats/formula.html) or
  [brms::brmsformula](https://paulbuerkner.com/brms/reference/brmsformula.html)
  (or one that can be coerced to those classes). A symbolic description
  of the model to be fitted. A formula must be provided for the
  distributional parameter `mu`, and may optionally be provided for
  other distributional parameters.

- ...:

  Additional arguments passed to `fn` method.

## Value

A `brmsprior` object.

## See also

Other prior:
[`epidist_family_prior()`](https://epidist.epinowcast.org/reference/epidist_family_prior.md),
[`epidist_family_prior.default()`](https://epidist.epinowcast.org/reference/epidist_family_prior.default.md),
[`epidist_family_prior.gengamma()`](https://epidist.epinowcast.org/reference/epidist_family_prior.gengamma.md),
[`epidist_family_prior.lognormal()`](https://epidist.epinowcast.org/reference/epidist_family_prior.lognormal.md),
[`epidist_model_prior()`](https://epidist.epinowcast.org/reference/epidist_model_prior.md),
[`epidist_model_prior.default()`](https://epidist.epinowcast.org/reference/epidist_model_prior.default.md),
[`epidist_model_prior.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_model_prior.epidist_meta_model.md),
[`epidist_prior()`](https://epidist.epinowcast.org/reference/epidist_prior.md)
