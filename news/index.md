# Changelog

## epidist 0.5.0

### Features

- `epidist` data objects now check themselves when they are modified.
  Every object also carries a shared `epidist_data` class with methods
  for subsetting, replacement,
  [`rbind()`](https://rdrr.io/r/base/cbind.html) and the `dplyr` verbs.
  These re-check the object and drop any `epidist` class whose
  requirements it no longer meets, warning about what was dropped and
  why. An object that still carries an `epidist` class is therefore a
  valid object of that class.
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  and results with no columns are exceptions, both documented in
  [`?epidist_data`](https://epidist.epinowcast.org/reference/epidist_data.md).
  See
  [`?epidist_data`](https://epidist.epinowcast.org/reference/epidist_data.md)
  and [\#399](https://github.com/epinowcast/epidist/issues/399).
- Dropped the checks that ran on objects which had already been checked
  when they were created. The `as_epidist_*()` and
  [`epidist_stancode()`](https://epidist.epinowcast.org/reference/epidist_stancode.md)
  methods now trust the class they dispatch on. See
  [\#399](https://github.com/epinowcast/epidist/issues/399).
- [`epidist_transform_data_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.md)
  now checks the object it builds for the marginal and naive models.
  That object was never checked before, which only showed once the check
  in
  [`epidist_stancode()`](https://epidist.epinowcast.org/reference/epidist_stancode.md)
  was removed. See
  [\#399](https://github.com/epinowcast/epidist/issues/399).

### Package

- Added
  [`simulate_dates()`](https://epidist.epinowcast.org/reference/simulate_dates.md),
  which turns simulated event times into the censored dates an analyst
  would receive.
- Removed the calls to unexported `brms` functions that
  `R CMD check --as-cran` flags. `R/brms-compat.R` now holds small
  internal helpers reproducing the narrow behaviour `epidist` relied on
  from `brms:::validate_family()`, `brms:::validate_formula()`,
  `brms:::validate_data()`, `brms:::dpar_bounds()` and
  `brms:::log_lik_weight()`. The helpers are written against the public
  `brms` interface rather than copied from `brms`.
  `tests/testthat/test-brms-compat.R` checks each one against the `brms`
  internal it replaces. Those checks are skipped on CRAN, since they
  reach into `brms` internals. Credit for the original behaviour goes to
  the `brms` authors. See
  [\#420](https://github.com/epinowcast/epidist/issues/420) and
  paul-buerkner/brms#1676.
- Removed the `Remotes` field from `DESCRIPTION` so dependencies resolve
  from CRAN. `cmdstanr` is now found through `Additional_repositories`
  and the development version of `brms` is no longer used. See
  [\#592](https://github.com/epinowcast/epidist/issues/592).
- Turned off evaluation of the approximate inference vignette. It uses
  `pathfinder`, which needs an unreleased `brms` fix. This release
  resolves `brms` from CRAN. See
  [\#579](https://github.com/epinowcast/epidist/issues/579).
- Added a `brms (>= 2.23.0)` floor, the version the compatibility
  helpers were checked against.
- Pointed the CI workflows at the Stan r-universe with
  `extra-repositories`. Dropping `Remotes` means `pak` can no longer
  resolve `cmdstanr`. `pak` does not read `Additional_repositories`.
- Raised the minimum R version to 4.1.0. The package uses the native
  pipe and the lambda shorthand. Both need R 4.1.0.
- Added the copyright holder role to Sam Abbott in `DESCRIPTION`.
- Guarded the shared test fits and the tests that use them so the suite
  runs without `cmdstanr`.
- Wrapped the
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) and
  [`epidist_diagnostics()`](https://epidist.epinowcast.org/reference/epidist_diagnostics.md)
  examples in `\donttest{}`. Both fit a model. They ran for 118 and 110
  seconds against CRAN’s 5 second guidance.
- Anchored the `brms` links in the documentation so `R CMD check` no
  longer reports Rd cross-references with missing package anchors.
- Dropped a stale `fix` entry from the declared global variables.
- Updated the `brms` documentation URL, which had moved.
- Added `cran-comments.md`.
- Rewrote the generic
  [`epidist_gen_log_lik()`](https://epidist.epinowcast.org/reference/epidist_gen_log_lik.md)
  method so it evaluates the `brms` log likelihood once per delay rather
  than once per delay per posterior draw. A single `brms` call already
  returns the cdf for every draw, so the results are cached and reused.
  The method also calls
  [`primarycensored::pcens_cdf()`](https://primarycensored.epinowcast.org/reference/pcens_cdf.html)
  directly instead of
  [`primarycensored::dpcens()`](https://primarycensored.epinowcast.org/reference/dprimarycensored.html),
  which revalidates the distribution function at random points on every
  call and so would defeat the cache. Cost is now linear rather than
  quadratic in the number of draws. For 500 draws this is around 80
  times faster, and the log likelihoods are unchanged. The guard that
  `dpcens()` applied when the delay upper bound exceeds the relative
  observation time is reproduced explicitly, since this no longer goes
  through `dpcens()`. Left truncation is carried through the rewritten
  path: the density is normalised over the interval from `delay_min` to
  the relative observation time. See
  [\#476](https://github.com/epinowcast/epidist/issues/476).

### Documentation

- Added an `extending-epidist` vignette covering why you might build
  your own model type, the six generics a model type implements, a
  worked example, and a table of the packages that already extend
  `epidist`.
- Precomputed the `ebola`, `faq` and `approx-inference` vignettes. All
  three fit models and need `cmdstanr`, so they were excluded from the
  build by `.Rbuildignore` and never reached anyone who installed the
  package. They are now knitted from a `.Rmd.orig` source into a
  committed `.Rmd` holding static output, so they ship without needing
  `cmdstanr` or a model fit at build time.
- Gave each precomputed vignette its own figure prefix. `ebola` and
  `approx-inference` both wrote to `figures/epidist-`, which would
  collide once more than one is precomputed.

### Package

- Made
  [`epidist_family_param()`](https://epidist.epinowcast.org/reference/epidist_family_param.md)
  internal. It is reached through
  [`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md),
  and a custom model supplies its family through
  [`epidist_family_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.md)
  instead. See [\#79](https://github.com/epinowcast/epidist/issues/79).
- Exported
  [`epidist_gen_log_lik()`](https://epidist.epinowcast.org/reference/epidist_gen_log_lik.md),
  which was the only one of the three post-processing generators not
  exported. See [\#79](https://github.com/epinowcast/epidist/issues/79).
- Made
  [`epidist_transform_data()`](https://epidist.epinowcast.org/reference/epidist_transform_data.md)
  internal. It is a wrapper that dispatches to
  [`epidist_transform_data_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.md),
  which is the generic an extension implements and which remains
  exported. See [\#79](https://github.com/epinowcast/epidist/issues/79).

### Models

- Added left truncation support via a `delay_min` parameter in
  [`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.md).
  This passes the `L` (left truncation) argument through to the
  `primarycensored` likelihood. The default of 0 reproduces the previous
  behaviour. See
  [\#588](https://github.com/epinowcast/epidist/issues/588) and
  [\#596](https://github.com/epinowcast/epidist/issues/596).

### CI

- Added a `render-vignettes` workflow that rebuilds the precomputed
  vignettes and opens a pull request with the result.

### Bug fixes

- Added a missing Jacobian adjustment to the latent model for
  observations whose primary and secondary censoring windows overlap.
  Without it the latent model did not target the same likelihood as the
  marginal model. Under daily censoring the affected observations are
  the zero-delay cases. See
  [\#606](https://github.com/epinowcast/epidist/issues/606).
- Declared `reformulas` in `Suggests` and skipped the `marginaleffects`
  integration test when it is absent. `insight` needs `reformulas` to
  read the formula of a `brmsfit`, but only suggests it, so the test
  failed on a clean library. See
  [\#601](https://github.com/epinowcast/epidist/issues/601).

### Documentation

- Added a `left-truncation` vignette showing how to use `delay_min`. See
  [\#596](https://github.com/epinowcast/epidist/issues/596).

### CI

- Passed the coverage report to `codecov/codecov-action` through `files`
  rather than `file`. `file` is not an input the action accepts, so with
  `disable_search` set it found no report and the `test-coverage` job
  failed on `main`.
- Pinned the `precommit` hooks to a revision whose lockfile uses
  `digest` 0.6.39. The tagged v0.4.3 lockfile pins `digest` 0.6.36,
  which calls `Calloc` and `Free`. Those were removed from the R API in
  R 4.5, so the hook environment failed to build and the `pre-commit`
  job failed on every pull request. See
  [\#578](https://github.com/epinowcast/epidist/issues/578).

### Documentation

- Documented installing from CRAN in the README, with `r-universe` as
  the route to the latest version.

## epidist 0.4.1

### Bug fixes

- Fixed Stan compilation failure with primarycensored \>= 1.4.0 by
  adding the new `L` (left truncation) parameter to the
  `primarycensored_lpmf` call in the marginal model. See
  [\#583](https://github.com/epinowcast/epidist/issues/583).
- Added `primarycensored (>= 1.4.0)` version bound to DESCRIPTION.
- Updated test expectations for changed primarycensored error handling.
- Re-enabled approximate inference vignette evaluation using dev brms
  with pathfinder path fix. See
  [\#579](https://github.com/epinowcast/epidist/issues/579).

### Package

- Load only required primarycensored Stan functions
  (`primarycensored_lpmf` and ODE/distribution helpers) with
  `pcd_load_stan_functions(dependencies = TRUE)` instead of loading all
  functions. See
  [\#582](https://github.com/epinowcast/epidist/issues/582).

### CI

- Extended `check-cmdstan` workflow to also check marginal model Stan
  syntax.

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
