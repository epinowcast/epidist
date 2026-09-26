# Changelog

## epidist 1.0.0

This is the first release of `epidist` on CRAN. `epidist` estimates
epidemiological delay distributions, such as the incubation period or
the delay from onset to report, using `brms`. It corrects for the common
biases in these data: interval censoring of the primary and secondary
events, right truncation, left truncation, and the dynamical bias from a
growing or shrinking epidemic. It provides a naive model, a latent model
that samples the unobserved event times, and a marginal model that
integrates them out through `primarycensored`. It also provides an
experimental meta model, which fits published summary estimates jointly
with individual level data and adjusts each summary for how its study
was estimated. Delays can follow a lognormal, gamma, Weibull or
generalised gamma distribution. Every distributional parameter can take
a `brms` formula, so delays can vary with covariates, over time or
between groups with partial pooling. Tools for preparing data, setting
priors, simulating data, summarising and plotting fitted delay
distributions, and passing them on to other packages complete the
workflow. The meta model is still experimental and its interface may
change.

### New features

- Added
  [`gengamma()`](https://epidist.epinowcast.org/reference/gengamma.md),
  a generalised gamma delay family in the Prentice parameterisation of
  [`flexsurv::dgengamma()`](http://chjackson.github.io/flexsurv-dev/reference/GenGamma.md),
  for the naive, latent, marginal and meta models. The Weibull and gamma
  are special cases and the lognormal is its limit. Closes
  [\#644](https://github.com/epinowcast/epidist/issues/644).

### Documentation

- The help for
  [`epidist_gen_meta_log_lik()`](https://epidist.epinowcast.org/reference/epidist_gen_meta_log_lik.md)
  and
  [`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md)
  now states the cost of
  [`log_lik()`](https://mc-stan.org/rstantools/reference/log_lik.html)
  and [`loo()`](https://mc-stan.org/loo/reference/loo.html) for meta
  model summary rows, and how `ndraws` reduces it. Closes
  [\#705](https://github.com/epinowcast/epidist/issues/705).
- Added examples to the `simulate_*()` functions and to
  [`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md),
  [`epidist_formula()`](https://epidist.epinowcast.org/reference/epidist_formula.md),
  [`epidist_prior()`](https://epidist.epinowcast.org/reference/epidist_prior.md)
  and
  [`epidist_stancode()`](https://epidist.epinowcast.org/reference/epidist_stancode.md).
- The package description cites the methods it implements.

### Bug fixes

- Removed the remaining lookups of unexported `brms` functions, so
  [`log_lik()`](https://mc-stan.org/rstantools/reference/log_lik.html),
  [`posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html)
  and
  [`posterior_epred()`](https://mc-stan.org/rstantools/reference/posterior_epred.html)
  use `epidist`’s own functions for every family. These cover the
  lognormal, gamma, Weibull, exponential and generalised gamma families.
  For any other family these functions now give an error.
- [`print()`](https://rdrr.io/r/base/print.html) of an
  `epidist_multivariate` object writes its header as output rather than
  a message, so
  [`suppressMessages()`](https://rdrr.io/r/base/message.html) no longer
  hides it.

### Package

- The vignettes are built with
  [`bookdown::html_vignette2`](https://pkgs.rstudio.com/bookdown/reference/html_document2.html),
  which cuts the installed size of the package by about 4Mb.
- Cited the `sierra_leone_ebola_data` source by its DOI and corrected
  its column descriptions.
- Removed the unused `BH`, `Rcpp`, `RcppEigen`, `pkgdown` and `usethis`
  suggested dependencies.

## epidist 0.5.0

This release adds a meta model for fitting to published summary
estimates, exponentially growing primary events, left truncation, and a
new set of tools for post-processing and plotting fitted models. It also
removes several functions and changes some interfaces, as listed below.

### Breaking changes

- `predict_delay_parameters()` and `predict_dpar()` are removed in
  favour of
  [`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md),
  and `add_mean_sd()` in favour of
  [`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md).
  See [\#471](https://github.com/epinowcast/epidist/issues/471).
- [`epidist_family_param()`](https://epidist.epinowcast.org/reference/epidist_family_param.md)
  and
  [`epidist_transform_data()`](https://epidist.epinowcast.org/reference/epidist_transform_data.md)
  are now internal. Extensions implement
  [`epidist_family_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.md)
  and
  [`epidist_transform_data_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.md)
  instead. See [\#79](https://github.com/epinowcast/epidist/issues/79).
- The `is_epidist_*()` predicates share one signature,
  `is_epidist_<class>(data)`, and no longer take `...`. Closes
  [\#706](https://github.com/epinowcast/epidist/issues/706).
- `epidist` data objects share an `epidist_data` class, checked by
  [`is_epidist_data()`](https://epidist.epinowcast.org/reference/is_epidist_data.md).
  They re-check themselves when modified and drop any class whose
  requirements they no longer meet, with a warning. See
  [`?epidist_data`](https://epidist.epinowcast.org/reference/epidist_data.md)
  and [\#399](https://github.com/epinowcast/epidist/issues/399).
- `cmdstanr` is no longer a suggested dependency. Tests and examples fit
  through `rstan`, and `cmdstanr` remains available as a `brms` backend.
  See [\#687](https://github.com/epinowcast/epidist/issues/687) and
  [\#688](https://github.com/epinowcast/epidist/issues/688).
- Now requires R 4.1.0, `brms` 2.23.0 and `primarycensored` 1.5.2. The
  `Remotes` field is removed, so all dependencies resolve from CRAN. See
  [\#592](https://github.com/epinowcast/epidist/issues/592) and
  [\#727](https://github.com/epinowcast/epidist/issues/727).

### New features

- Added the meta model,
  [`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md),
  which fits published summary estimates jointly with individual level
  data. Each summary is forward modelled from the study’s own estimation
  procedure, so estimates that did not adjust for censoring or
  truncation still contribute unbiased information. It is experimental
  and its interface may change. See
  [\#620](https://github.com/epinowcast/epidist/issues/620).

- Added
  [`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md),
  [`epidist_estimates_summaries()`](https://epidist.epinowcast.org/reference/epidist_estimates_summaries.md),
  [`epidist_estimates_parameters()`](https://epidist.epinowcast.org/reference/epidist_estimates_parameters.md),
  [`epidist_estimates_epireview()`](https://epidist.epinowcast.org/reference/epidist_estimates_epireview.md)
  and
  [`as_epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.md)
  for preparing published estimates. They cover means, standard
  deviations, quantiles, standard errors, fitted distribution parameters
  and summaries with a covariance, and check the inputs for common
  problems. See
  [\#620](https://github.com/epinowcast/epidist/issues/620).

- Added
  [`epidist_gen_meta_log_lik()`](https://epidist.epinowcast.org/reference/epidist_gen_meta_log_lik.md)
  and
  [`epidist_gen_meta_predict()`](https://epidist.epinowcast.org/reference/epidist_gen_meta_predict.md),
  so
  [`log_lik()`](https://mc-stan.org/rstantools/reference/log_lik.html),
  [`loo()`](https://mc-stan.org/loo/reference/loo.html) and posterior
  predictions work for meta model fits. See
  [\#620](https://github.com/epinowcast/epidist/issues/620).

- Added
  [`epidist_meta_leave_one_out()`](https://epidist.epinowcast.org/reference/epidist_meta_leave_one_out.md),
  which refits a meta model once per study with that study held out and
  reports how the delay mean and standard deviation change. Closes
  [\#642](https://github.com/epinowcast/epidist/issues/642).

- Added
  [`simulate_study()`](https://epidist.epinowcast.org/reference/simulate_study.md),
  which applies a published study’s observation and estimation procedure
  to a simulated line list. Closes
  [\#672](https://github.com/epinowcast/epidist/issues/672).

- The meta model can estimate the growth rate of a study from `NA` or
  uncertain `growth_rate` values, sharing it with individual level data.
  Closes [\#678](https://github.com/epinowcast/epidist/issues/678).

- Added an exponentially growing primary event distribution with
  `primary = "expgrowth"` for the latent, marginal and meta models. The
  growth rate is a distributional parameter, so it takes a formula and
  prior. See [\#489](https://github.com/epinowcast/epidist/issues/489)
  and [\#618](https://github.com/epinowcast/epidist/issues/618).

- Added left truncation through a `delay_min` argument to
  [`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.md),
  also supported by the meta model. See
  [\#588](https://github.com/epinowcast/epidist/issues/588) and
  [\#596](https://github.com/epinowcast/epidist/issues/596).

- Added
  [`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md),
  [`add_delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md),
  [`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md),
  [`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)
  and
  [`delay_summary_draws()`](https://epidist.epinowcast.org/reference/delay_summary_draws.md)
  for posterior draws of the delay distribution and its mean, standard
  deviation and quantiles.
  [`epidist_newdata()`](https://epidist.epinowcast.org/reference/epidist_newdata.md)
  builds the `newdata` they need. See
  [\#280](https://github.com/epinowcast/epidist/issues/280),
  [\#471](https://github.com/epinowcast/epidist/issues/471) and
  [\#667](https://github.com/epinowcast/epidist/issues/667).

- Added
  [`plot_events()`](https://epidist.epinowcast.org/reference/plot_events.md),
  [`plot_delays()`](https://epidist.epinowcast.org/reference/plot_delays.md),
  and [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  [`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  methods for delay draws.
  [`plot_delays()`](https://epidist.epinowcast.org/reference/plot_delays.md)
  also gives a posterior predictive check of a fitted model. See
  [\#670](https://github.com/epinowcast/epidist/issues/670),
  [\#689](https://github.com/epinowcast/epidist/issues/689) and
  [\#743](https://github.com/epinowcast/epidist/issues/743).

- Added a
  [`distspec::as_dist_spec()`](https://epiforecasts.io/distspec/reference/as_dist_spec.html)
  method for fitted models. See
  [\#712](https://github.com/epinowcast/epidist/issues/712).

- Added
  [`simulate_dates()`](https://epidist.epinowcast.org/reference/simulate_dates.md),
  which turns simulated event times into censored dates.

- Exported
  [`epidist_gen_log_lik()`](https://epidist.epinowcast.org/reference/epidist_gen_log_lik.md),
  and made its generic method linear in the number of posterior draws.
  See [\#79](https://github.com/epinowcast/epidist/issues/79),
  [\#476](https://github.com/epinowcast/epidist/issues/476) and
  [\#646](https://github.com/epinowcast/epidist/issues/646).

- [`epidist_prior()`](https://epidist.epinowcast.org/reference/epidist_prior.md)
  no longer warns about user priors on valid `brms` parameters, and
  lists unmatched priors clearly. See
  [\#483](https://github.com/epinowcast/epidist/issues/483).

- Added
  [`nonparametric()`](https://epidist.epinowcast.org/reference/nonparametric.md),
  a delay distribution family with no parametric form, for the marginal
  and meta models. The delay sits on a grid of bins, with its
  probability at the right edge of each bin, and is written as the
  discrete time hazard of each bin, using the non-parametric
  distributions of `primarycensored`. The logit hazards are given by a
  formula over the bins, such as a spline over the delay or a random
  intercept per bin with `~ (1 | bin)`. The default is the spline, or
  the random intercept when fewer than three bins have a free hazard.
  Each coefficient of that formula is a distributional parameter, so it
  takes a `brms` formula and prior. A covariate in the `mu` formula
  shifts the logit hazard of every bin, a proportional odds model for
  the hazard, and a covariate in the formula of a coefficient changes
  the shape of the delay. In the meta model a study that fully adjusted
  for censoring can only report the mean and standard deviation of the
  whole delay, because the family has no density. See
  [`vignette("nonparametric")`](https://epidist.epinowcast.org/articles/nonparametric.md)
  and [\#557](https://github.com/epinowcast/epidist/issues/557).

- The package lifecycle is now maturing rather than experimental. The
  meta model is still experimental. See
  [\#781](https://github.com/epinowcast/epidist/issues/781).

### Bug fixes

- Added a missing Jacobian adjustment to the latent model for
  observations whose primary and secondary windows overlap. See
  [\#606](https://github.com/epinowcast/epidist/issues/606).
- [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) now
  restores the `PKG_CPPFLAGS` and `PKG_LIBS` environment variables that
  `rstan` leaves set. See
  [\#532](https://github.com/epinowcast/epidist/issues/532).
- The generic
  [`epidist_gen_log_lik()`](https://epidist.epinowcast.org/reference/epidist_gen_log_lik.md)
  method now normalises over `delay_min` when there is no right
  truncation. Closes
  [\#646](https://github.com/epinowcast/epidist/issues/646).
- [`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)
  no longer passes on the `brms` warning about the infinite observation
  times `epidist` uses. Closes
  [\#718](https://github.com/epinowcast/epidist/issues/718).
- Delay draws keep their class through common `dplyr` verbs, so
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) still
  dispatches. Closes
  [\#721](https://github.com/epinowcast/epidist/issues/721).
- The meta model log likelihood no longer advances the RNG stream.
  Closes [\#750](https://github.com/epinowcast/epidist/issues/750).
- Removed calls to unexported `brms` functions. See
  [\#420](https://github.com/epinowcast/epidist/issues/420).

### Documentation

- Added vignettes on the meta model, applied to published Ebola
  estimates, on left truncation, and on extending `epidist`. See
  [\#596](https://github.com/epinowcast/epidist/issues/596) and
  [\#620](https://github.com/epinowcast/epidist/issues/620).
- All vignettes that fit models are now precomputed, so they ship with
  the package without needing a model fit at build time. See
  [\#619](https://github.com/epinowcast/epidist/issues/619) and
  [\#688](https://github.com/epinowcast/epidist/issues/688).
- Reworked the getting started vignette around the package’s simulation
  and plotting tools. See
  [\#736](https://github.com/epinowcast/epidist/issues/736).
- Added the meta model to the model guide vignette. See
  [\#620](https://github.com/epinowcast/epidist/issues/620) and
  [\#709](https://github.com/epinowcast/epidist/issues/709).
- Documented the return value of every exported function, and installing
  from CRAN and r-universe in the README. See
  [\#702](https://github.com/epinowcast/epidist/issues/702).

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
- Fixed a bug in `predict_delay_parameters()` where it couldn’t detect
  brms families when used directly. See
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
distributions. It uses [`brms`](https://paulbuerkner.com/brms/) to
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
