# Changelog

## epidist 0.4.0

### Package

- Enforce line length and use cli for latent prior checks. See
  [\#580](https://github.com/epinowcast/epidist/issues/580).
- Removed CodeDepends from DESCRIPTION dependencies.

### Documentation

- Restructured pkgdown reference with higher-level categories. See
  [\#574](https://github.com/epinowcast/epidist/issues/574).
- Updated FAQ to recommend pp_check with expanded data. See
  [\#575](https://github.com/epinowcast/epidist/issues/575).
- Clarified weight parameter documentation in
  [`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.md).
  See [\#565](https://github.com/epinowcast/epidist/issues/565).
- Fixed pathfinder parameter usage in approximate inference vignette.
  See [\#573](https://github.com/epinowcast/epidist/issues/573).

## epidist 0.3.1

Hotfix release to patch a change in how the `grepl` function works in
new versions of R.

## epidist 0.3.0

This release adds support for a wider range of distributions in the
marginal model, improves documentation with new vignettes and FAQ
sections, enhances the getting started guide with clearer examples of
model comparison, and fixes several bugs related to parameter bounds and
likelihood calculations.

### Models

- Added Stan-side support for fitting all distributions supported by
  `primarycensored` in the marginal model. See
  [\#540](https://github.com/epinowcast/epidist/issues/540).
- Added R-side analytical likelihood support for Lognormal, Gamma, and
  Weibull distributions. See
  [\#540](https://github.com/epinowcast/epidist/issues/540).

### Package

- Remove caching of vignettes. See
  [\#533](https://github.com/epinowcast/epidist/issues/533).

### Documentation

- Added a new vignette “Guide to the statistical models implemented in
  epidist”. See
  [\#514](https://github.com/epinowcast/epidist/issues/514).
- Added a new FAQ section showcasing how to use the `posterior` package
  with `epidist` models, particularly for working with random variables
  (`rvars`) to propagate uncertainty in calculations. See
  [\#547](https://github.com/epinowcast/epidist/issues/547).
- Added a new FAQ section on how to use the `marginaleffects` package
  with `epidist` models. See
  [\#547](https://github.com/epinowcast/epidist/issues/547).
- Reduced the focus on simulating data in the getting started vignette
  to make it more accessible. See
  [\#549](https://github.com/epinowcast/epidist/issues/549).
- Made the entry to the package friendlier with clearer examples and
  improved documentation. See
  [\#549](https://github.com/epinowcast/epidist/issues/549).
- Added a schematic to explain right truncation more clearly to the
  getting started vignette. See
  [\#549](https://github.com/epinowcast/epidist/issues/549).
- Added a comparison of fitting naive and marginal models in the getting
  started vignette to highlight the importance of accounting for biases.
  See [\#549](https://github.com/epinowcast/epidist/issues/549).
- Added examples showing how to extract estimated parameters and plot
  them against true values to evaluate model performance. See
  [\#549](https://github.com/epinowcast/epidist/issues/549).

### Bugs

- Fixed a vector length issue for censoring that was causing problems in
  some likelihood calls. See
  [\#540](https://github.com/epinowcast/epidist/issues/540).
- Fixed a bug in the preprocessing of the Weibull family. See
  [\#540](https://github.com/epinowcast/epidist/issues/540).
- Fixed a bug where bounds were not set for mu parameters in custom
  families. See
  [\#549](https://github.com/epinowcast/epidist/issues/549).
- Fixed a bug in
  [`predict_delay_parameters()`](https://epidist.epinowcast.org/reference/predict_delay_parameters.md)
  where it couldn’t detect brms families when used directly. See
  [\#549](https://github.com/epinowcast/epidist/issues/549).

## epidist 0.2.0

This release adds a new marginal model based on `primarycensored` which
provides a more efficient approach for fitting delay distributions
compared to the existing latent model. We’ve also improved data handling
by adding support for aggregated data across all models, added
comprehensive examples using real world data, and enhanced documentation
based on user feedback. The package has also undergone significant
internal improvements including generalised Stan reparameterisation and
improved data transformation methods.

As part of this release we have moved from
[@athowes](https://github.com/athowes) maintaining the package (who led
the initial package development, implementation of the S3
infrastructure, implementation of the core models, and wrote the first
versions of the getting started vignette, Ebola case study, FAQ section,
and the approximate inference vignette) to
[@seabbs](https://github.com/seabbs) maintaining the package.

### Models

- Added a marginalised likelihood model based on `primarycensored`. This
  can be specified using
  [`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.md).
  This is currently limited to Weibull, log-normal, and gamma
  distributions with uniform primary censoring but this will be
  generalised in future releases. See
  [\#426](https://github.com/epinowcast/epidist/issues/426).
- Added user settable primary event priors to the latent model. See
  [\#474](https://github.com/epinowcast/epidist/issues/474).
- Added a marginalised likelihood to the latent model. See
  [\#474](https://github.com/epinowcast/epidist/issues/474).
- Added a `weight` argument to
  [`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.md)
  to allow for weighted data (for example count data) to be used in the
  marginal model. See
  [\#509](https://github.com/epinowcast/epidist/issues/509).
- Added a `epidist_aggregate_data` method to
  [`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.md)
  to allow straightforward use of the marginal model with aggregated
  data. See [\#510](https://github.com/epinowcast/epidist/issues/510).
- Added a `epidist_aggregate_data` method to
  [`as_epidist_latent_model()`](https://epidist.epinowcast.org/reference/as_epidist_latent_model.md)
  to allow straightforward use of the latent model with aggregated data.
  See [\#510](https://github.com/epinowcast/epidist/issues/510).
- Added a `epidist_aggregate_data` method to
  [`as_epidist_naive_model()`](https://epidist.epinowcast.org/reference/as_epidist_naive_model.md)
  to allow straightforward use of the naive model with aggregated data.
  See [\#510](https://github.com/epinowcast/epidist/issues/510).
- Updated the naive model to internally transform the data to be
  optimally aggregated as for the marginal model. See
  [\#510](https://github.com/epinowcast/epidist/issues/510).

### Package

- Remove the default method for
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).
  See [\#473](https://github.com/epinowcast/epidist/issues/473).
- Added `enforce_presence` argument to
  [`epidist_prior()`](https://epidist.epinowcast.org/reference/epidist_prior.md)
  to allow for priors to be specified if they do not match existing
  parameters. See
  [\#474](https://github.com/epinowcast/epidist/issues/474).
- Added a `merge` argument to
  [`epidist_prior()`](https://epidist.epinowcast.org/reference/epidist_prior.md)
  to allow for not merging user and package priors. See
  [\#474](https://github.com/epinowcast/epidist/issues/474).
- Generalised the Stan reparametrisation feature to work across all
  distributions without manual specification by generating Stan code
  with `brms` and then extracting the reparameterisation. See
  [\#474](https://github.com/epinowcast/epidist/issues/474).
- Added a `transform_data` S3 method to allow for data to be transformed
  for specific models. This is specifically useful for the marginal
  model at the moment as it allows reducing the data to its unique
  strata. See [\#474](https://github.com/epinowcast/epidist/issues/474).
- Added new `epidist_aggregate_data` class to handle pre-aggregated line
  list data. See
  [\#510](https://github.com/epinowcast/epidist/issues/510).
- Added a
  [`as_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.md)
  method for `epidist_linelist_data` objects to allow for easy
  conversion to aggregate data. See
  [\#510](https://github.com/epinowcast/epidist/issues/510).
- Added a
  [`as_epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_linelist_data.md)
  method for `epidist_aggregate_data` objects to allow for easy
  conversion to linelist data. See
  [\#510](https://github.com/epinowcast/epidist/issues/510).
- Added an example dataset `sierra_leone_ebola_data` to the package. See
  [\#510](https://github.com/epinowcast/epidist/issues/510).
- Added examples to most functions to show usage of the package. See
  [\#510](https://github.com/epinowcast/epidist/issues/510).
- Added improved documentation explaining how the
  [`epidist_transform_data()`](https://epidist.epinowcast.org/reference/epidist_transform_data.md)
  methods work for the marginal and naive models. See
  [\#510](https://github.com/epinowcast/epidist/issues/510).

### Documentation

- Brings the README into line with `epinowcast` standards. See
  [\#467](https://github.com/epinowcast/epidist/issues/467).
- Switched over to using the marginal model as default in the
  documentation. See
  [\#426](https://github.com/epinowcast/epidist/issues/426).
- Added a helper functions for new variables to avoid code duplication
  in vignettes. See
  [\#426](https://github.com/epinowcast/epidist/issues/426).
- Improved the Ebola case study vignette to use truncated data and to
  reduce the focus on exploratory data analysis. See
  [\#510](https://github.com/epinowcast/epidist/issues/510).

### Bugs

- Switched to using a patched of `primarycensored` that doesn’t make use
  of `size()`. This fixes some Mac compilation edge cases. See
  [\#524](https://github.com/epinowcast/epidist/issues/524).

## epidist 0.1.0

This is the first minor release of `epidist` intended for early test
users of the package. As some features may change, the package is marked
as experimental. We expect to release a stable 1.0.0 version shortly.

The `epidist` package implements models for epidemiological delay
distributions. It uses [`brms`](http://paulbuerkner.com/brms/) to
perform Bayesian inference.

One data format is currently available:

1.  The [linelist
    data](https://epidist.epinowcast.org/reference/index.html#linelist-data)
    format

Two statistical models are currently available:

1.  The [naive
    model](https://epidist.epinowcast.org/reference/index.html#naive-model):
    which models the delay directly using `brms`
2.  The [latent
    model](https://epidist.epinowcast.org/reference/index.html#latent-model):
    which implements a latent variable model to correct for biases in
    the data

The package is readily extensible to additional models via an
[S3](https://adv-r.hadley.nz/s3.html) class based system. In particular,
model fitting with \[epidist()\] is possible using S3 classes for
custom:

1.  [Families](https://epidist.epinowcast.org/reference/index.html#family)
2.  [Formula](https://epidist.epinowcast.org/reference/index.html#formula)
3.  [Prior
    distributions](https://epidist.epinowcast.org/reference/index.html#prior-distributions)
4.  [Stan
    code](https://epidist.epinowcast.org/reference/index.html#stan-code)

We provide functionality for
[post-processing](https://epidist.epinowcast.org/reference/index.html#postprocess).
Alternatively, users may directly use `tidybayes` for specific families.

Three vignettes are available. There is also a [frequently asked
questions](https://epidist.epinowcast.org/articles/faq.html) section.
