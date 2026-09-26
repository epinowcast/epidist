# Non-parametric delay distribution family

A delay distribution with no parametric form, for use with the marginal
and meta models. Pass it as the `family` argument of
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) in
the same way as
[`brms::lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html).

The delay sits on a grid of \\K\\ bins with boundaries \\b_0 \< b_1 \<
\dots \< b_K\\. All the probability of bin \\k\\ is placed at its right
edge \\b_k\\, so the delay distribution function is a step function.
This is the discrete hazard distribution of
[`primarycensored::pdiscretehazard()`](https://primarycensored.epinowcast.org/reference/pdiscretehazard.html),
and the likelihood is the `primarycensored` one for that distribution.

The distribution is written in terms of the discrete time hazard of each
bin, the probability that the delay ends in bin \\k\\ given that it did
not end before it. The hazard of the last bin is 1, so all the delays
end by \\b_K\\. The logit hazards of the other bins are a linear
predictor over the bins, \$\$\mathrm{logit}(h_k) = \mu + \sum_q B\_{kq}
\theta_q,\$\$ where the basis \\B\\ comes from `formula` evaluated on
the bins.

## Usage

``` r
nonparametric(formula = NULL, boundaries = NULL)
```

## Arguments

- formula:

  A one sided formula for the logit hazards over the bins, see Details.
  The default, `NULL`, is a spline over the delay.

- boundaries:

  A numeric vector of at least four strictly increasing bin boundaries,
  \\b_0\\ to \\b_K\\. The default, `NULL`, is set by
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md)
  from the data, with a bin for every whole delay from 0 up to the
  longest delay in the data, and at least four bins, that is
  `seq(-1, max(3, max_delay))`. The last boundary must be at least as
  long as the longest observed delay.

## Value

A `brmsfamily` object for use as the `family` argument of
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

## The hazard formula

`formula` is a one sided formula in two variables of the bins: `delay`,
the right edge \\b_k\\ of each bin, and `bin`, a factor with one level
per bin. It takes the terms of a `brms` formula that make sense on the
bins:

- parametric terms such as `delay`, `I(delay^2)` or `bin`,

- smooths such as `s(delay)`, `s(delay, bs = "ps")` or `t2(delay)`,
  built with
  [`mgcv::smoothCon()`](https://rdrr.io/pkg/mgcv/man/smoothCon.html) as
  `brms` builds them,

- a random intercept per bin, `(1 | bin)`, which is the smooth
  `s(bin, bs = "re")`.

`~ 1` gives the same hazard in every bin, a geometric delay. The
default, `NULL`, is `~ s(delay, k = min(10, K - 1))`, a thin plate
regression spline over the delay, which smooths the hazard over
neighbouring bins. With fewer than three free bins it is `~ (1 | bin)`.

The intercept of the formula is dropped, because \\\mu\\ is the
intercept. Each column of the basis is centred over the bins, so \\\mu\\
is the mean logit hazard, and scaled, so that each coefficient is on the
scale of the logit hazard.

## Parameters

Each coefficient \\\theta_q\\ is a distributional parameter, so it
appears in the output of
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) and
takes a `brms` formula and prior in the same way as `mu`:

- `h<i>b` is the coefficient of the \\i\\th unpenalised column, such as
  a parametric term or the linear part of a spline.

- `h<j>sd` is the standard deviation of the \\j\\th penalised term, such
  as the wiggly part of a spline or the random intercept per bin.

- `h<i>z` is the \\i\\th standardised coefficient of the penalised
  terms, so the coefficient is `h<j>sd * h<i>z` for the term \\j\\ the
  column belongs to. This is the non-centred form `brms` uses for
  smooths and random effects.

The `coefficients` element of the `np` element of the family built by
[`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md)
names the term of each coefficient.

## Covariates

A covariate in the `mu` formula shifts the logit hazard of every bin by
the same amount, which is a proportional odds model for the hazard. A
covariate in the formula of a coefficient changes the shape of the
hazard over the bins, a non-proportional effect. For example with
`formula = ~ delay`, `bf(mu ~ age_group, h1b ~ age_group)` gives each
age group its own intercept and slope of the logit hazard over the
delay, as `delay * age_group` would.

## Priors

The default priors are `normal(logit(1 / K), 1.5)` on the intercept of
`mu`, centred on the hazard of the first bin when every bin is equally
likely, `normal(0, 2)` on each `h<i>b`, `normal(0, 1)` on the log of
each `h<j>sd`, as `primarycensored` uses for the spread of the logit
hazards, and `std_normal()` on each `h<i>z`. Set others with the `prior`
argument of
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

## See also

The non-parametric section of
[`vignette("model")`](https://epidist.epinowcast.org/articles/model.md)
for the model,
[`vignette("nonparametric")`](https://epidist.epinowcast.org/articles/nonparametric.md)
for a worked example, and the `primarycensored` article on fitting
non-parametric delays
(<https://primarycensored.epinowcast.org/articles/fitting-nonparametric-delays.html>)
for the censored likelihood.

Other family:
[`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md),
[`epidist_family_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.md),
[`epidist_family_model.default()`](https://epidist.epinowcast.org/reference/epidist_family_model.default.md),
[`epidist_family_param()`](https://epidist.epinowcast.org/reference/epidist_family_param.md),
[`epidist_family_param.default()`](https://epidist.epinowcast.org/reference/epidist_family_param.default.md),
[`epidist_family_param.gengamma()`](https://epidist.epinowcast.org/reference/epidist_family_param.gengamma.md),
[`epidist_family_param.nonparametric()`](https://epidist.epinowcast.org/reference/epidist_family_param.nonparametric.md),
[`gengamma()`](https://epidist.epinowcast.org/reference/gengamma.md)

## Examples

``` r
nonparametric(boundaries = -1:10)
#> 
#> Family: nonparametric 
#> Link function: identity 
#> 
nonparametric(~ (1 | bin), boundaries = -1:10)
#> 
#> Family: nonparametric 
#> Link function: identity 
#> 
nonparametric(~ s(delay, bs = "ps", k = 6), boundaries = c(-1:5, 7, 10))
#> 
#> Family: nonparametric 
#> Link function: identity 
#> 
```
