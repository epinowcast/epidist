# epidist 0.5.0

## Models

- Added an exponentially growing primary event distribution to the latent model.
`as_epidist_latent_model(primary = "expgrowth")` makes the growth rate a distributional parameter, so it takes a `brms` formula and prior and can vary by covariate.
See #489 and #618.

## Features

- Added `epidist_newdata()`, which builds the `newdata` needed to predict from a fitted model.
It expands the variables you give it into a grid and adds the response and observation process variables the model uses, so you no longer have to know the column names each model expects.
The defaults give the delay distribution with no censoring and no truncation, and arguments set the censoring windows, the relative observation time and the minimum delay.
The result works with `brms::posterior_epred()`, `predict_delay_parameters()` and the `tidybayes` draw functions.
See `?epidist_newdata` and #280.
- Added `delay_parameter_draws()` and `add_delay_parameter_draws()`, which return posterior draws of the delay distribution parameters in the long format used by `tidybayes`.
- Added `delay_parameter_draws()` and `add_delay_parameter_draws()`, which return posterior draws of the delay distribution parameters in the long format used by `tidybayes`.
The draws come back with `.row`, `.chain`, `.iteration` and `.draw` columns alongside the columns of `newdata`.
`predict_delay_parameters()` and `predict_dpar()` are removed in their favour.
See #471.
- Added `add_summaries()`, which adds the mean, the standard deviation and quantiles of the delay distribution implied by each draw of its parameters.
It uses the analytic solution for the lognormal, gamma and Weibull families, and simulates from any other family, so it works for every family `brms` can predict from.
`add_mean_sd()` is removed in its favour.
See #471.
- Added `epidist_strata()`, which returns one row of the model data per unique combination of the variables that predict the delay distribution parameters.
Passing it to `add_delay_parameter_draws()` draws each set of parameters once rather than once per observation.
See #471.
- `epidist` data objects now check themselves when they are modified.
Every object also carries a shared `epidist_data` class with methods for subsetting, replacement, `rbind()` and the `dplyr` verbs.
These re-check the object and drop any `epidist` class whose requirements it no longer meets, warning about what was dropped and why.
An object that still carries an `epidist` class is therefore a valid object of that class.
`dplyr::group_by()` and results with no columns are exceptions, both documented in `?epidist_data`.
See `?epidist_data` and #399.
- Dropped the checks in `epidist_stancode()` and in the conversions between linelist and aggregate data, which ran on objects that had already been checked.
The conversions from linelist data to a model still check their input, because `new_epidist_linelist_data()` does not.
See #399.
- `epidist_transform_data_model()` now checks the object it builds for the marginal and naive models.
That object was never checked before, which only showed once the check in `epidist_stancode()` was removed.
See #399.
## Package

- Reworded the message `as_epidist_marginal_model()` gives when it sets relative observation times to `Inf`.
The message now names `relative_obs_time` and `orig_relative_obs_time`.
It explains that the impact on accuracy is small because these observation times cause very limited right truncation.
It also points at `obs_time_threshold` for users who do not want the behaviour.
See #536.
- `epidist_prior()` no longer warns about user priors on parameters that are in the model but not in the `epidist` default set.
The warning now checks user priors against the `brms` default priors for the model as well as the `epidist` ones, so a prior on a regression coefficient no longer looks unmatched.
See #483.
- Simplified the internals of prior handling.
`.replace_prior()` now only merges priors, with the warning about unmatched priors moved to `.warn_unmatched_prior()` and the `merge` argument handled in `epidist_prior()`.
The latent model checks of the event window priors moved to `R/latent_model.R` and dispatch on the data class, so a model can now state its own prior requirements.
The returned priors, and the Stan code they produce, are unchanged.
See #483.
- The warning about unmatched priors now lists each prior with the parameter it applies to, rather than printing the internal join it came from.
See #483.
- Documented the return value of every exported function.
- Declared `scales` in `Suggests`, which the FAQ vignette loads but nothing declared.
- Fixed four typos that `inst/WORDLIST` was masking, corrected two moved URLs, title cased the `Title` field, and set `Language: en-GB`.
- Fixed `inst/CITATION` rendering the year as `NULL`.
- `epidist()` now restores the `PKG_CPPFLAGS` and `PKG_LIBS` environment variables it found before fitting.
The `rstan` backend sets both while compiling and never restores them.
The leaked `PKG_CPPFLAGS` made the next `pkgbuild::has_build_tools()` check fail, which printed a spurious `fatal error: cmath: No such file or directory` before the model compiled and fitted successfully.
See #532.
- Added `simulate_dates()`, which turns simulated event times into the censored dates an analyst would receive.
- Removed the calls to unexported `brms` functions that `R CMD check --as-cran` flags.
`R/brms-compat.R` now holds small internal helpers reproducing the narrow behaviour `epidist` relied on from `brms:::validate_family()`, `brms:::validate_formula()`, `brms:::validate_data()`, `brms:::dpar_bounds()` and `brms:::log_lik_weight()`.
The helpers are written against the public `brms` interface rather than copied from `brms`.
`tests/testthat/test-brms-compat.R` checks each one against the `brms` internal it replaces.
Those checks are skipped on CRAN, since they reach into `brms` internals.
Credit for the original behaviour goes to the `brms` authors.
See #420 and paul-buerkner/brms#1676.
- Removed the `Remotes` field from `DESCRIPTION` so dependencies resolve from CRAN.
`cmdstanr` is now found through `Additional_repositories` and the development version of `brms` is no longer used.
See #592.
- Turned off evaluation of the approximate inference vignette.
It uses `pathfinder`, which needs an unreleased `brms` fix.
This release resolves `brms` from CRAN.
See #579.
- Added a `brms (>= 2.23.0)` floor, the version the compatibility helpers were checked against.
- Pointed the CI workflows at the Stan r-universe with `extra-repositories`.
Dropping `Remotes` means `pak` can no longer resolve `cmdstanr`.
`pak` does not read `Additional_repositories`.
- Raised the minimum R version to 4.1.0.
The package uses the native pipe and the lambda shorthand.
Both need R 4.1.0.
- Added the copyright holder role to Sam Abbott in `DESCRIPTION`.
- Guarded the shared test fits and the tests that use them so the suite runs without `cmdstanr`.
- Wrapped the `epidist()` and `epidist_diagnostics()` examples in `\donttest{}`.
Both fit a model.
They ran for 118 and 110 seconds against CRAN's 5 second guidance.
- Anchored the `brms` links in the documentation so `R CMD check` no longer reports Rd cross-references with missing package anchors.
- Dropped a stale `fix` entry from the declared global variables.
- Updated the `brms` documentation URL, which had moved.
- Added `cran-comments.md`.
- Rewrote the generic `epidist_gen_log_lik()` method so it evaluates the `brms` log likelihood once per delay rather than once per delay per posterior draw.
A single `brms` call already returns the cdf for every draw, so the results are cached and reused.
The method also calls `primarycensored::pcens_cdf()` directly instead of `primarycensored::dpcens()`, which revalidates the distribution function at random points on every call and so would defeat the cache.
Cost is now linear rather than quadratic in the number of draws.
For 500 draws this is around 80 times faster, and the log likelihoods are unchanged. The guard that `dpcens()` applied when the delay upper bound exceeds the relative observation time is reproduced explicitly, since this no longer goes through `dpcens()`.
Left truncation is carried through the rewritten path: the density is normalised over the interval from `delay_min` to the relative observation time.
See #476.

## Documentation

- Added an `extending-epidist` vignette covering why you might build your own model type, the six generics a model type implements, a worked example, and a table of the packages that already extend `epidist`.
- Precomputed the `ebola`, `faq` and `approx-inference` vignettes.
All three fit models and need `cmdstanr`, so they were excluded from the build by `.Rbuildignore` and never reached anyone who installed the package.
They are now knitted from a `.Rmd.orig` source into a committed `.Rmd` holding static output, so they ship without needing `cmdstanr` or a model fit at build time.
- Gave each precomputed vignette its own figure prefix.
`ebola` and `approx-inference` both wrote to `figures/epidist-`, which would collide once more than one is precomputed.

## Package

- Made `epidist_family_param()` internal.
It is reached through `epidist_family()`, and a custom model supplies its family through `epidist_family_model()` instead.
See #79.
- Exported `epidist_gen_log_lik()`, which was the only one of the three post-processing generators not exported.
See #79.
- Made `epidist_transform_data()` internal.
It is a wrapper that dispatches to `epidist_transform_data_model()`, which is the generic an extension implements and which remains exported.
See #79.

## Models

- Added left truncation support via a `delay_min` parameter in `as_epidist_marginal_model()`.
This passes the `L` (left truncation) argument through to the `primarycensored` likelihood.
The default of 0 reproduces the previous behaviour.
See #588 and #596.

## CI

- Added a `render-vignettes` workflow that rebuilds the precomputed vignettes and opens a pull request with the result.

## Models

- Added the meta model, for fitting to summarised and potentially biased published estimates, jointly with individual level data.
Published estimates are forward modelled from the study's own estimation procedure, so summaries that did not adjust for right truncation or that treated interval censored data as continuous can still contribute unbiased information, given correct metadata describing what each study did.
The meta model is experimental and its interface may still change.
See `as_epidist_meta_model()`.
See #620.
- Added `as_epidist_estimates_data()` for preparing published summary estimates, with documentation of the study metadata the meta model needs.
See #620.
- The meta model supports studies that stopped collecting at a calendar date through the `trunc_design` field of `as_epidist_estimates_data()`, which weights the estimand by the follow up available to each delay rather than conditioning on a single cohort cutoff.
See #620.
- The meta model supports midpoint imputation, where a study assigned each delay to the centre of the interval it was observed in, as `cens_adjusted` code 3.
See #620.
- A standard error supplied for a quantile row of `as_epidist_estimates_data()` is now interpreted on the delay scale, as studies report it, and the row is fitted on that scale against the implied quantile.
See #620.
- `as_epidist_estimates_data()` now rejects a reported quantile at or beyond the largest delay its study could have seen, which would otherwise contribute a constant to the likelihood rather than information.
See #620.
- The meta model now fits the summaries a study computed from the same delays jointly rather than as independent terms.
A mean with a standard deviation uses the asymptotic bivariate normal of the pair, and a set of quantiles uses the multinomial mass of the delays falling between them.
This removes the over-weighting of a study reporting a median with an interquartile range.
One observation is now a group of summaries, so `log_lik()` and `loo()` work at that level.
See #620.
- A quantile reported by a study that took integer date differences from a cohort now costs three distribution function evaluations rather than one per grid cell, because the grid is normalised by the distribution function at its top.
On the quantile reporting studies of the meta vignette this cuts the evaluations they need per gradient from 480 to 36.
The shortcut does not apply to a study that stopped collecting at a calendar date, which reweights each cell before renormalising and so keeps the full grid.
See #620.
- The implied quantile of a continuous estimand is inverted exactly where the family quantile function exists and refined by Newton steps otherwise, so covariance rows with quantile members and quantile rows with a delay scale standard error are no longer limited by the quadrature spacing.
See #620.
- The meta model gained several further speed ups.
Softmax normalisation of the cohort grid mass is replaced by division where the normaliser is already known.
A zero growth rate accrual design skips its exponential terms.
R post-processing batches and caches implied summaries across grouped rows sharing a design.
See #620.
- The quadrature resolution used for truncated continuous moments is now set by `options(epidist.meta_n_quad = )`, defaulting to 100 intervals.
This is the floor of the resolution chosen per study, see below.
See #620.
- Added `as_epidist_multivariate()`, which summarises draws of a set of parameters by their mean vector and covariance matrix, over an optional trajectory index.
Passing the result to `as_epidist_estimates_data()` gives a vector of reported summaries with the covariance between them, fitted as a multivariate normal.
This is the format we recommend when a study cannot share its delays, because it keeps the correlation between the quantities it reports.
Draws of the natural parameters of a fitted distribution are pushed through to the summaries the distribution implies, so no linearisation is used.
See #620.
- Added `epidist_estimates_summaries()` and `epidist_estimates_parameters()`, which take one study's contribution in the shape it reported it.
`epidist_estimates_parameters()` converts the parameters of a distribution a study fitted into the summaries that distribution implies, carrying any reported parameter standard errors onto that scale by the delta method as a covariance over the summaries, which is fitted jointly and carries the study's information about its parameters exactly.
The family a study fitted need not match the family being fitted to it.
See #620.
- `as_epidist_estimates_data()` combines contributions passed in a list, so studies reporting in different shapes assemble into one object.
See #620.
- The meta model gained a fifth censoring adjustment code, `cens_adjusted = 4`, for a study that placed the primary event at the midpoint of its window and integrated the secondary interval.
Its estimand is that of `cens_adjusted = 2` moved down the delay axis by half a primary window, so its reported mean loses the half window bias while its spread keeps the primary window's variance.
See #620.
- The meta model supports left truncation through `delay_min`, on both individual level rows and summary rows.
A study that only counted delays above a minimum has every implied summary conditioned on the delay exceeding it.
See #596 and #620.
- A single quantile reported by a study that summarised integer day delays is now fitted as the cell in which the empirical distribution function crossed its probability, the exact event a rounded quantile stands for, rather than with a multinomial on the continuity corrected grid whose claimed precision keeps growing with the sample size.
Two quantiles reported at the same value are accepted and merged into one cell, and `as_epidist_estimates_data()` warns when a large study reports several such quantiles, whose joint likelihood is still overconfident.
See #620.
- A multinomial cell that underflows is floored rather than sent to zero, so the R and Stan log likelihoods are both finite for a badly misfitting draw and `loo()` keeps working.
See #620.
- `as_epidist_estimates_data()` refuses more summaries from a fitted family than it has parameters, and a covariance over reported summaries that is singular to within a relative eigenvalue of 1e-4, because such a row charges any error in the implied summaries against a vanishing eigenvalue.
See #620.
- `as_epidist_estimates_data()` warns when the relative standard error of a reported standard deviation, at the kurtosis its mean and standard deviation imply under a lognormal delay, exceeds a quarter, which is where the normal sampling likelihood of a standard deviation stops being calibrated.
See #620.
- The meta model takes a `primary` argument for its individual level rows, as the marginal model does.
With `primary = "expgrowth"` the growth rate of primary events is estimated as the `pgrowth` distributional parameter.
Summary rows are unchanged and keep the `growth_rate` metadata of their study as a known tilt.
See #620.
- Added an `epidist_model_prior()` method for the meta model, which centres the intercept prior of `mu` on the log of the median mean the studies reported, with a standard deviation of 1 on the log scale.
Without it a Gamma or Weibull fit to summaries alone took the `brms` default, which is centred on the response column and so on a delay of zero, because that column is a placeholder on summary rows.
See #620.
- Added an `epidist_newdata()` method for the meta model, which builds an individual level row with the same arguments as the marginal model method, so predicting from a meta model fit no longer means copying a summary row out of the model data.
See #620.
- `as_epidist_estimates_data()` now rejects a reported mean at or beyond the observation time of a study that did not adjust for right truncation, a standard error of zero, a standard deviation of zero, and a `cens_adjusted` code that is not a whole number from 0 to 4.
It warns, rather than messages, when no `trunc_adjusted` column is supplied and a study is therefore assumed to have adjusted for right truncation.
The `growth_rate` documentation now separates its within window tilt from the accrual weight it applies under `trunc_design = "accrual"`, and points to the `primary = "expgrowth"` option of the marginal model for individual level data.
See #620.
- The midpoint imputation codes of the meta model (`cens_adjusted` 3 and 4) now move `delay_min` with the midpoint shift, so a study that dropped reported delays below a minimum is left truncated at the right point.
Before this the implied mean was 5 to 15% low for code 4 and up to 24% high for code 3 with a wide secondary window.
See #620.
- A study that adjusted for right truncation and counted only delays above `delay_min` now has its moments left truncated analytically, so they describe the same estimand as its quantile rows and no longer depend on `max_delay`.
For a heavy tailed delay the implied standard deviation was 6 to 17% low before this.
See #620.
- The accrual weight on the discrete grid now cuts each reporting cell at the primary windows it spans, so it is exact for unequal censoring windows whenever the collection window is a multiple of `pwindow`.
Weighting at the cell's lower edge put the implied mean 12 to 36% low for a daily primary and weekly secondary window.
See #620.
- The meta model now chooses its quadrature resolution per study from the spread the study reported, so that the node spacing is at most a quarter of that spread, with `options(epidist.meta_n_quad)` as the floor and 2000 intervals as a cap the option lifts when set above it.
The number travels with each summary row as its `n_quad` slot rather than being compiled into the Stan code.
Before this a fixed 100 intervals over the default `max_delay` put the implied standard deviation of a delay with a coefficient of variation of 0.05 out by a factor of two and pinned its kurtosis at the floor, which made a reported standard deviation almost infinitely precise.
`as_epidist_estimates_data()` now warns only when the cap leaves a study unresolved.
See #620.
- A draw whose implied moments overflow is rejected on every meta model moment row rather than returning `NaN` for an ungrouped standard deviation.
The analytic moments themselves now reject such a draw in Stan and return the failure vector in R, so a covariance row rejects a very wide draw instead of carrying an infinite gradient, and the posterior predictive of such a draw is `NA` rather than a `NaN` standard error.
See #620.
- The meta model no longer evaluates the primary censored distribution function at grid or quadrature nodes deep in the lower tail of a narrow delay, where the Stan function of `primarycensored` has a finite value with a `NaN` gradient.
A node whose log distribution function is certainly below -100, decided from a closed form bound on the parameters, is treated as holding no mass in both implementations.
CmdStan's gradient diagnostic now passes on every design of the meta vignette at a log standard deviation of 0.03, where four of them could not start before.
See #620.
- Fixed two gaps in the Stan mirror of a left truncated midpoint code: the delay scale quantile path sized its nodes from the unshifted `delay_min`, and the Newton refinement of an implied quantile normalised from `delay_min` rather than the moved left truncation point of a code 4 study.
The Stan crossing cell of a single integer day quantile also takes its binomial upper tail through the accurate side, which had put it 5e-3 away from R at a poorly fitting draw.
See #620.
- `as_epidist_estimates_data()` rejects a `cens_adjusted = 4` study whose `delay_min` plus half its `pwindow` reaches the grid cutoff.
See #620.

## Documentation

- Added a "The meta model" section to the model guide vignette, with the forward model and sampling likelihoods used for published summary estimates.
See #620.
- Added a vignette showcasing the meta model on simulated data.
Its case study builds nine studies, each applying a different estimation procedure to the same line list, so the recovery result tests every bias the model adjusts for.
See #620.
- The meta vignette is now precomputed from a `.Rmd.orig` source, so it ships with the package.
See #619 and #620.
- The meta vignette now works through the published Ebola onset to death estimates collated by `epireview`.
It adjusts for the phase of the outbreak each estimate was made in, taking the retrospective studies as the reference so the population level estimate is the one least affected by right truncation, and reports the phase bias as a marginal effect.
It reports the population level posterior of the Gamma shape and scale alongside the natural mean and standard deviation, and compares the result with a modern re-analysis of one of the same line lists.
See #620.

## Bug fixes

- `.delay_family()` now strips the `meta_` prefix alongside `latent_` and `marginal_`.
Without it `add_summaries()` could not find the delay distribution of a meta model fit, because the family is named `meta_gamma` rather than `gamma`.
See #620.
- Added a missing Jacobian adjustment to the latent model for observations whose primary and secondary censoring windows overlap.
Without it the latent model did not target the same likelihood as the marginal model.
Under daily censoring the affected observations are the zero-delay cases.
See #606.
- Declared `reformulas` in `Suggests` and skipped the `marginaleffects` integration test when it is absent.
`insight` needs `reformulas` to read the formula of a `brmsfit`, but only suggests it, so the test failed on a clean library.
See #601.

## Documentation

- Added a `left-truncation` vignette showing how to use `delay_min`.
See #596.

## CI

- Passed the coverage report to `codecov/codecov-action` through `files` rather than `file`.
`file` is not an input the action accepts, so with `disable_search` set it found no report and the `test-coverage` job failed on `main`.
- Pinned the `precommit` hooks to a revision whose lockfile uses `digest` 0.6.39.
The tagged v0.4.3 lockfile pins `digest` 0.6.36, which calls `Calloc` and `Free`.
Those were removed from the R API in R 4.5, so the hook environment failed to build and the `pre-commit` job failed on every pull request.
See #578.

## Documentation

- Documented installing from CRAN in the README, with `r-universe` as the route to the latest version.

# epidist 0.4.1

## Bug fixes

- Fixed Stan compilation failure with primarycensored >= 1.4.0 by adding the
  new `L` (left truncation) parameter to the `primarycensored_lpmf` call in
  the marginal model. See #583.
- Added `primarycensored (>= 1.4.0)` version bound to DESCRIPTION.
- Updated test expectations for changed primarycensored error handling.
- Re-enabled approximate inference vignette evaluation using dev brms with
  pathfinder path fix. See #579.

## Package

- Load only required primarycensored Stan functions
  (`primarycensored_lpmf` and ODE/distribution helpers) with
  `pcd_load_stan_functions(dependencies = TRUE)` instead of loading all
  functions. See #582.

## CI

- Extended `check-cmdstan` workflow to also check marginal model Stan syntax.

# epidist 0.4.0

## Package

- Enforce line length and use cli for latent prior checks. See #580.
- Removed CodeDepends from DESCRIPTION dependencies.

## Documentation

- Restructured pkgdown reference with higher-level categories. See #574.
- Updated FAQ to recommend pp_check with expanded data. See #575.
- Clarified weight parameter documentation in `as_epidist_marginal_model()`. See #565.
- Fixed pathfinder parameter usage in approximate inference vignette. See #573.

# epidist 0.3.1

Hotfix release to patch a change in how the `grepl` function works in new versions of R.

# epidist 0.3.0

This release adds support for a wider range of distributions in the marginal model, improves documentation with new vignettes and FAQ sections, enhances the getting started guide with clearer examples of model comparison, and fixes several bugs related to parameter bounds and likelihood calculations.

## Models

- Added Stan-side support for fitting all distributions supported by `primarycensored` in the marginal model. See #540.
- Added R-side analytical likelihood support for Lognormal, Gamma, and Weibull distributions. See #540.

## Package

- Remove caching of vignettes. See #533.

## Documentation

- Added a new vignette "Guide to the statistical models implemented in epidist". See #514.
- Added a new FAQ section showcasing how to use the `posterior` package with `epidist` models, particularly for working with random variables (`rvars`) to propagate uncertainty in calculations. See #547.
- Added a new FAQ section on how to use the `marginaleffects` package with `epidist` models. See #547.
- Reduced the focus on simulating data in the getting started vignette to make it more accessible. See #549.
- Made the entry to the package friendlier with clearer examples and improved documentation. See #549.
- Added a schematic to explain right truncation more clearly to the getting started vignette. See #549.
- Added a comparison of fitting naive and marginal models in the getting started vignette to highlight the importance of accounting for biases. See #549.
- Added examples showing how to extract estimated parameters and plot them against true values to evaluate model performance. See #549.

## Bugs

- Fixed a vector length issue for censoring that was causing problems in some likelihood calls. See #540.
- Fixed a bug in the preprocessing of the Weibull family. See #540.
- Fixed a bug where bounds were not set for mu parameters in custom families. See #549.
- Fixed a bug in `predict_delay_parameters()` where it couldn't detect brms families when used directly. See #549.

# epidist 0.2.0

This release adds a new marginal model based on `primarycensored` which provides a more efficient approach for fitting delay distributions compared to the existing latent model. We've also improved data handling by adding support for aggregated data across all models, added comprehensive examples using real world data, and enhanced documentation based on user feedback. The package has also undergone significant internal improvements including generalised Stan reparameterisation and improved data transformation methods.

As part of this release we have moved from @athowes maintaining the package (who led the initial package development, implementation of the S3 infrastructure, implementation of the core models, and wrote the first versions of the getting started vignette, Ebola case study, FAQ section, and the approximate inference vignette) to @seabbs maintaining the package.

## Models

- Added a marginalised likelihood model based on `primarycensored`. This can be specified using `as_epidist_marginal_model()`. This is currently limited to Weibull, log-normal, and gamma distributions with uniform primary censoring but this will be generalised in future releases. See #426.
- Added user settable primary event priors to the latent model. See #474.
- Added a marginalised likelihood to the latent model. See #474.
- Added a `weight` argument to `as_epidist_marginal_model()` to allow for weighted data (for example count data) to be used in the marginal model. See #509.
- Added a `epidist_aggregate_data` method to `as_epidist_marginal_model()` to allow straightforward use of the marginal model with aggregated data. See #510.
- Added a `epidist_aggregate_data` method to `as_epidist_latent_model()` to allow straightforward use of the latent model with aggregated data. See #510.
- Added a `epidist_aggregate_data` method to `as_epidist_naive_model()` to allow straightforward use of the naive model with aggregated data. See #510.
- Updated the naive model to internally transform the data to be optimally aggregated as for the marginal model. See #510.

## Package

- Remove the default method for `epidist()`. See #473.
- Added `enforce_presence` argument to `epidist_prior()` to allow for priors to be
  specified if they do not match existing parameters. See #474.
- Added a `merge` argument to `epidist_prior()` to allow for not merging user and package priors. See #474.
- Generalised the Stan reparametrisation feature to work across all distributions without manual specification by generating Stan code with `brms` and then extracting the reparameterisation. See #474.
- Added a `transform_data` S3 method to allow for data to be transformed for specific models. This is specifically useful for the marginal model at the moment as it allows reducing the data to its unique strata. See #474.
- Added new `epidist_aggregate_data` class to handle pre-aggregated line list data. See #510.
- Added a `as_epidist_aggregate_data()` method for `epidist_linelist_data` objects to allow for easy conversion to aggregate data. See #510.
- Added a `as_epidist_linelist_data()` method for `epidist_aggregate_data` objects to allow for easy conversion to linelist data. See #510.
- Added an example dataset `sierra_leone_ebola_data` to the package. See #510.
- Added examples to most functions to show usage of the package. See #510.
- Added improved documentation explaining how the `epidist_transform_data()` methods work for the marginal and naive models. See #510.

## Documentation

- Brings the README into line with `epinowcast` standards. See #467.
- Switched over to using the marginal model as default in the documentation. See #426.
- Added a helper functions for new variables to avoid code duplication in vignettes. See #426.
- Improved the Ebola case study vignette to use truncated data and to reduce the focus on exploratory data analysis. See #510.

## Bugs

- Switched to using a patched of `primarycensored` that doesn't make use of `size()`. This fixes some Mac compilation edge cases. See #524.

# epidist 0.1.0

This is the first minor release of `epidist` intended for early test users of the package.
As some features may change, the package is marked as experimental.
We expect to release a stable 1.0.0 version shortly.

The `epidist` package implements models for epidemiological delay distributions.
It uses [`brms`](http://paulbuerkner.com/brms/) to perform Bayesian inference.

One data format is currently available:

1. The [linelist data](https://epidist.epinowcast.org/reference/index.html#linelist-data) format

Two statistical models are currently available:

1. The [naive model](https://epidist.epinowcast.org/reference/index.html#naive-model): which models the delay directly using `brms`
2. The [latent model](https://epidist.epinowcast.org/reference/index.html#latent-model): which implements a latent variable model to correct for biases in the data

The package is readily extensible to additional models via an [S3](https://adv-r.hadley.nz/s3.html) class based system.
In particular, model fitting with [epidist()] is possible using S3 classes for custom:

1. [Families](https://epidist.epinowcast.org/reference/index.html#family)
2. [Formula](https://epidist.epinowcast.org/reference/index.html#formula)
3. [Prior distributions](https://epidist.epinowcast.org/reference/index.html#prior-distributions)
4. [Stan code](https://epidist.epinowcast.org/reference/index.html#stan-code)

We provide functionality for [post-processing](https://epidist.epinowcast.org/reference/index.html#postprocess).
Alternatively, users may directly use `tidybayes` for specific families.

Three vignettes are available.
There is also a [frequently asked questions](https://epidist.epinowcast.org/articles/faq.html) section.
