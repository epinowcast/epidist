# epidist 0.5.0

This release adds a meta model for fitting to published summary estimates, exponentially growing primary events, left truncation, and a new set of tools for post-processing and plotting fitted models.
It also removes several functions and changes some interfaces, as listed below.

- Added `nonparametric()`, a delay distribution family with no parametric form, for the marginal and meta models.
The delay sits on a grid of bins, with its probability at the right edge of each bin, and is written as the discrete time hazard of each bin, using the non-parametric distributions of `primarycensored`.
The logit hazards follow a random walk by default, or independent random effects with `hazard_model = "re"`.
The `mu` formula shifts the logit hazard of every bin, so `mu ~ 1 + age_group` fits a separate delay per age group under a proportional odds model for the hazard.
In the meta model a study that fully adjusted for censoring can only report the mean and standard deviation of the whole delay, because the family has no density.
See `vignette("nonparametric")` and #557.

- Added an exponentially growing primary event distribution to the latent model.
`as_epidist_latent_model(primary = "expgrowth")` makes the growth rate a distributional parameter, so it takes a `brms` formula and prior and can vary by covariate.
See #489 and #618.
- Added left truncation support via a `delay_min` parameter in `as_epidist_marginal_model()`.
This passes the `L` (left truncation) argument through to the `primarycensored` likelihood.
The default of 0 reproduces the previous behaviour.
See #588 and #596.
- Added the meta model, for fitting to summarised and potentially biased published estimates, jointly with individual level data.
Published estimates are forward modelled from the study's own estimation procedure, so summaries that did not adjust for right truncation or that treated interval censored data as continuous can still contribute unbiased information, given correct metadata describing what each study did.
The meta model is experimental and its interface may still change.
See `as_epidist_meta_model()`.
See #620.
- Added `as_epidist_estimates_data()` for preparing published summary estimates, with documentation of the study metadata the meta model needs.
See #620.
- The meta model supports studies that stopped collecting at a calendar date through the `trunc_design` field of `as_epidist_estimates_data()`, which weights the estimand by the follow up available to each delay rather than conditioning on a single cohort cutoff.
See #620.
- The accrual weight on the grid of a study that stopped collecting at a calendar date now treats a partial last primary window explicitly, weighting the cases it holds by the follow up available over the part of the window inside the collection period.
It was exact only when `relative_obs_time` was a multiple of `pwindow`, and put the implied mean 27% low for weekly primary and secondary windows with a collection window of 30 days at a growth rate of 0.2.
Closes #680.
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
- Several quantiles reported by a study that summarised integer day delays are now fitted with the exact joint likelihood of the crossings they stand for.
The cumulative counts at the days the quantiles name form a Markov chain of binomial steps on the uncorrected grid distribution function, each reported quantile is a box on two of them, and the probability of every box holding is a forward pass over the counts, kept to a band around the most likely path so that it costs about the sample size to the power one and a half per edge.
Coincident quantiles are two constraints at one edge.
The multinomial on the continuity corrected grid that fitted such a set before claimed a standard error five times too small at a thousand delays, where the exact likelihood is a box of parameters rather than a peak.
The warning of `as_epidist_estimates_data()` about several integer day quantiles from more than 100 delays is removed, and the model data carries a `meta_group_lower` array with the lower bounds of the boxes.
`EPIDIST_META_CALIBRATION=true` now also fits forty replicates of a study reporting its quartiles at thirty and at a hundred delays, where the 90% intervals covered the truth 34 to 35 times in 40.
Closes #675.
- A multinomial cell that underflows is floored rather than sent to zero, so the R and Stan log likelihoods are both finite for a badly misfitting draw and `loo()` keeps working.
See #620.
- `as_epidist_estimates_data()` refuses more summaries from a fitted family than it has parameters, and a covariance over reported summaries that is singular to within a relative eigenvalue of 1e-4, because such a row charges any error in the implied summaries against a vanishing eigenvalue.
See #620.
- `as_epidist_estimates_data()` warns when the relative standard error of a reported standard deviation, at the kurtosis its mean and standard deviation imply under a lognormal delay, exceeds a quarter, which is where the normal sampling likelihood of a standard deviation stops being calibrated.
See #620.
- The meta model takes a `primary` argument for its individual level rows, as the marginal model does.
With `primary = "expgrowth"` the growth rate of primary events is estimated as the `pgrowth` distributional parameter.
Summary rows keep the `growth_rate` metadata of their study as a known tilt, unless the study estimates its rate.
See #620.
- Added an `epidist_model_prior()` method for the meta model, which puts a `normal(1, 1)` prior on the intercept of `mu` where it is on the log scale, the scale of the lognormal family prior.
The centre is fixed rather than taken from the reported values, because a prior chosen from the data would put the posterior of a small review where the data already sit.
The same method puts a half normal prior with a standard deviation of 0.25 on the between study standard deviation of any group level term, where the `brms` default is a half Student t with scale 2.5.
Closes #683.
Without it a Gamma or Weibull fit to summaries alone took the `brms` default, which is centred on the response column and so on a delay of zero, because that column is a placeholder on summary rows.
See #620.
- Added an `epidist_newdata()` method for the meta model, which builds an individual level row with the same arguments as the marginal model method, so predicting from a meta model fit no longer means copying a summary row out of the model data.
See #620.
- `as_epidist_estimates_data()` now rejects a reported mean at or beyond the observation time of a study that did not adjust for right truncation, a standard error of zero, a standard deviation of zero, and a `cens_adjusted` code that is not a whole number from 0 to 4.
It warns, rather than messages, when no `trunc_adjusted` column is supplied and a study is therefore assumed to have adjusted for right truncation.
The `growth_rate` documentation now separates its within window tilt from the accrual weight it applies under `trunc_design = "accrual"`, and points to the `primary = "expgrowth"` option of the marginal model for individual level data.
See #620.
- `as_epidist_estimates_data()` accepts `NA` censoring windows for a study that fully adjusted for censoring (`cens_adjusted = 1`), since none of its estimands read them, and rejects them with a message naming the study for every other code.
See #620.
- The advisory messages of `as_epidist_estimates_data()` are now two short sentences naming the studies, and the input row where a single summary is meant, and point at a new Checks section of its documentation that carries the reasoning.
They run once, when the estimates are built, rather than again when the object is passed to `as_epidist_meta_model()`.
See #620.
- The advisory messages of `as_epidist_estimates_data()` now say what each problem is in plain language, run once on the studies combined in a list rather than once per study, and are followed by a single pointer to the Checks section, with `simulate_study()` leaving them to the combined object and `advise = FALSE` skipping them.
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
- The crossing cell likelihood of a single integer day quantile is now taken as the difference of two binomial tails on the side where both are small, with each tail summed term by term once it is far out, and as a sum over the count below the cell where the difference still underflows, in R and in Stan.
Before this the difference cancelled or underflowed to `-Inf` when the implied distribution put the reported quantile far into its tail, which stopped chains initialising on the Ebola fit of the meta vignette, and Stan's binomial distribution function had non finite partial derivatives in the same region, which trapped a chain of the shared test fixture in one run in four.
CmdStan's gradient diagnostic now matches finite differences at every probed point.
See #620.
- `as_epidist_estimates_data()` rejects a `cens_adjusted = 4` study whose `delay_min` plus half its `pwindow` reaches the grid cutoff.
See #620.
- The meta model intercept prior for the lognormal family is now on the log scale, as it is for a log link.
The lognormal `mu` has an identity link but is `meanlog`, so it was treated as the delay itself.
A calibration check of a study reporting a mean with its standard error from 25 delays found it, where the 90% intervals covered the truth about half of the time.
See #620.
- Added opt in simulation checks of the meta model: `EPIDIST_META_RECOVERY=true` fits one meta model per censoring adjustment code and truncation design, and `EPIDIST_META_CALIBRATION=true` fits forty replicates of two study designs and checks interval coverage and the rank of the truth.
See #620.
- Added `simulate_study()`, which applies the observation and estimation procedure of one published study to a simulated line list and returns the summaries that study would have reported as an `epidist_estimates_data` object.
It covers every censoring adjustment code, both truncation designs, a minimum delay and a subsample, and reports a mean and standard deviation, quantiles, a mean with a standard error, or a multivariate mean and standard deviation with their bootstrap covariance.
Closes #672.
- The meta model now fits every summary a study with a continuous estimand reports as one multivariate normal, with the sampling covariance of its mean, standard deviation and quantiles derived from the implied distribution.
Before, the mean and standard deviation of a study were fitted separately from its quantiles, which counted a study reporting a mean, a standard deviation and quartiles about twice for the location, and a mean with a median 1.5 times at a study size of 100.
A study that reported integer date differences still has the two kinds fitted separately, because its quantiles are discrete statistics, so report its mean and standard deviation and drop its quantiles.
Closes #676.
- Summary rows of the meta model can now estimate their growth rate.
An `NA` `growth_rate` in `as_epidist_estimates_data()` makes the study use the `pgrowth` distributional parameter, the parameter that `primary = "expgrowth"` estimates from individual level data, so a line list from the same outbreak can inform the rate a published summary is corrected with.
A new `growth_rate_sd` column treats a reported rate as a normal prior on that parameter rather than as a fixed number.
The meta model adds `pgrowth ~ 0 + study` to the formula where a study estimates its rate, unless a `pgrowth` formula is given, and `epidist_model_prior()` sets the per study priors and a `normal(0, 0.25)` default for the rest.
The R and Stan implementations read the rate of each posterior draw.
`epidist_formula()` now lets a model add its own formulas before the remaining distributional parameters are given an intercept.
Closes #678.
## Breaking changes

- `predict_delay_parameters()` and `predict_dpar()` are removed in favour of `delay_parameter_draws()`, and `add_mean_sd()` in favour of `add_summaries()`.
See #471.
- `epidist_family_param()` and `epidist_transform_data()` are now internal.
Extensions implement `epidist_family_model()` and `epidist_transform_data_model()` instead.
See #79.
- The `is_epidist_*()` predicates share one signature, `is_epidist_<class>(data)`, and no longer take `...`.
Closes #706.
- `epidist` data objects share an `epidist_data` class, checked by `is_epidist_data()`.
They re-check themselves when modified and drop any class whose requirements they no longer meet, with a warning.
See `?epidist_data` and #399.
- `cmdstanr` is no longer a suggested dependency.
Tests and examples fit through `rstan`, and `cmdstanr` remains available as a `brms` backend.
See #687 and #688.
- Now requires R 4.1.0, `brms` 2.23.0 and `primarycensored` 1.5.2.
The `Remotes` field is removed, so all dependencies resolve from CRAN.
See #592 and #727.

## New features

- Added the meta model, `as_epidist_meta_model()`, which fits published summary estimates jointly with individual level data.
Each summary is forward modelled from the study's own estimation procedure, so estimates that did not adjust for censoring or truncation still contribute unbiased information.
It is experimental and its interface may change.
See #620.
- Added `as_epidist_estimates_data()`, `epidist_estimates_summaries()`, `epidist_estimates_parameters()`, `epidist_estimates_epireview()` and `as_epidist_multivariate()` for preparing published estimates.
They cover means, standard deviations, quantiles, standard errors, fitted distribution parameters and summaries with a covariance, and check the inputs for common problems.
See #620.
- Added `epidist_gen_meta_log_lik()` and `epidist_gen_meta_predict()`, so `log_lik()`, `loo()` and posterior predictions work for meta model fits.
See #620.
- Added `simulate_study()`, which applies a published study's observation and estimation procedure to a simulated line list.
Closes #672.
- The meta model can estimate the growth rate of a study from `NA` or uncertain `growth_rate` values, sharing it with individual level data.
Closes #678.
- Added an exponentially growing primary event distribution with `primary = "expgrowth"` for the latent, marginal and meta models.
The growth rate is a distributional parameter, so it takes a formula and prior.
See #489 and #618.
- Added left truncation through a `delay_min` argument to `as_epidist_marginal_model()`, also supported by the meta model.
See #588 and #596.
- Added `delay_parameter_draws()`, `add_delay_parameter_draws()`, `add_summaries()`, `epidist_strata()` and `delay_summary_draws()` for posterior draws of the delay distribution and its mean, standard deviation and quantiles.
`epidist_newdata()` builds the `newdata` they need.
See #280, #471 and #667.
- Added `plot_events()`, `plot_delays()`, and `plot()` and `ggplot2::autoplot()` methods for delay draws.
`plot_delays()` also gives a posterior predictive check of a fitted model.
See #670, #689 and #743.
- Added a `distspec::as_dist_spec()` method for fitted models.
See #712.
- Added `simulate_dates()`, which turns simulated event times into censored dates.
- Exported `epidist_gen_log_lik()`, and made its generic method linear in the number of posterior draws.
See #79, #476 and #646.
- `epidist_prior()` no longer warns about user priors on valid `brms` parameters, and lists unmatched priors clearly.
See #483.

## Bug fixes

- Added a missing Jacobian adjustment to the latent model for observations whose primary and secondary windows overlap.
See #606.
- `epidist()` now restores the `PKG_CPPFLAGS` and `PKG_LIBS` environment variables that `rstan` leaves set.
See #532.
- The generic `epidist_gen_log_lik()` method now normalises over `delay_min` when there is no right truncation.
Closes #646.
- `delay_parameter_draws()` no longer passes on the `brms` warning about the infinite observation times `epidist` uses.
Closes #718.
- Delay draws keep their class through common `dplyr` verbs, so `plot()` still dispatches.
Closes #721.
- Removed calls to unexported `brms` functions.
See #420.

## Documentation

- Added vignettes on the meta model, applied to published Ebola estimates, on left truncation, and on extending `epidist`.
See #596 and #620.
- All vignettes that fit models are now precomputed, so they ship with the package without needing a model fit at build time.
See #619 and #688.
- Reworked the getting started vignette around the package's simulation and plotting tools.
See #736.
- Added the meta model to the model guide vignette.
See #620 and #709.
- Documented the return value of every exported function, and installing from CRAN and r-universe in the README.
See #702.

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
