# Convert an object to an `epidist_meta_model` object

Creates an `epidist_meta_model` object from individual level data,
published summary estimates, or a mix of the two. This enables fitting a
single delay distribution to all of the evidence available using
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

## Usage

``` r
as_epidist_meta_model(data = NULL, estimates = NULL, ...)
```

## Arguments

- data:

  An `epidist_linelist_data` or `epidist_aggregate_data` object of
  individual level observations, an `epidist_estimates_data` object of
  published summary estimates, or `NULL`.

- estimates:

  An `epidist_estimates_data` object of published summary estimates, or
  `NULL`.

- ...:

  Additional arguments passed to methods.

## Value

An object of class `epidist_meta_model`.

## Details

The meta model is experimental. Its interface may still change in future
releases.

Individual level rows use the same likelihood as the marginal model (see
[`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.md)),
imported from the
[primarycensored](https://primarycensored.epinowcast.org/) package.
Summary rows are instead forward modelled. Given the delay distribution,
the model works out what the study's own estimation procedure would have
converged to, and fits the reported value to that. Published estimates
that did not adjust for right truncation, or that treated interval
censored data as continuous, can therefore still contribute unbiased
information. That holds only where the metadata describing what each
study did is correct. It is usually the analyst's judgement rather than
something the study reported, so state it explicitly and vary it in a
sensitivity analysis.
[`vignette("model")`](https://epidist.epinowcast.org/articles/model.md)
gives the forward model, the sampling likelihoods and the accuracy of
the approximations, and
[`vignette("meta")`](https://epidist.epinowcast.org/articles/meta.md)
works through a simulated and a real example.

At least one of `data` and `estimates` must be supplied. Study level
heterogeneity is specified through the `brms` formula in
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md), for
example `mu ~ 1 + (1 | study)`, rather than through this function.
Individual level rows are labelled `"individual"` in the `study` column
so that they form their own level of any such term.

## What this means in practice

Summaries that one study computed from the same delays are correlated,
so they are fitted jointly. Two are grouped when they agree on every
column of
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
other than the summary itself, and a summary supplied with its own `se`
is fitted alone. A study that reported integer date differences
(`cens_adjusted` 0 or 3) is the exception: its mean and standard
deviation form one group and its quantiles another. One observation is
therefore a group rather than a single reported value, so `log_lik()`
and [`loo::loo()`](https://mc-stan.org/loo/reference/loo.html) report
per group, and `loo` only compares fits to the same studies and the same
mix of individual and summary rows. See
[`vignette("faq")`](https://epidist.epinowcast.org/articles/faq.md).
[`epidist_meta_leave_one_out()`](https://epidist.epinowcast.org/reference/epidist_meta_leave_one_out.md)
asks the study level question instead, refitting with each study held
out.

The sampling standard errors are plug in quantities that depend on the
parameters, so allow for genuine differences between studies with a term
such as `mu ~ 1 + (1 | study)` rather than relying on them alone. Supply
a reported `se` in
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
for quantiles read off a fitted distribution rather than the empirical
data. Where a study reported integer date differences, keep its mean and
standard deviation and drop its quantiles.
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
warns for the studies the approximations serve least well, and documents
the two settings, `max_delay` and `options(epidist.meta_n_quad = )`,
that trade accuracy against speed.

## Advanced: an estimated growth rate

A summary row tilts its primary event, and weights the follow up of an
accrual design, by the `growth_rate` of its study. Where that rate is
`NA` in
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md),
or was given there with a `growth_rate_sd`, the study estimates it as
the `pgrowth` distributional parameter instead. That is the parameter
`primary = "expgrowth"` estimates from individual level rows, so the two
can share it. The model adds `pgrowth ~ 0 + study` unless a `pgrowth`
formula is given, and
[`epidist_model_prior()`](https://epidist.epinowcast.org/reference/epidist_model_prior.md)
sets the priors from the reported rates. Summaries carry little
information about the rate on their own, so share the coefficient with
rows that do inform it, for example `pgrowth ~ 1` with a line list from
the same outbreak.
[`vignette("model")`](https://epidist.epinowcast.org/articles/model.md)
gives the details.

## See also

[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
for preparing the summary estimates, and for the checks and the settings
the approximations here depend on.

Other meta_model:
[`as_epidist_meta_model.NULL()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.NULL.md),
[`as_epidist_meta_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_aggregate_data.md),
[`as_epidist_meta_model.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_estimates_data.md),
[`as_epidist_meta_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_linelist_data.md),
[`assert_epidist.epidist_meta_model()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_meta_model.md),
[`epidist_family_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.epidist_meta_model.md),
[`epidist_formula_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_meta_model.md),
[`epidist_meta_leave_one_out()`](https://epidist.epinowcast.org/reference/epidist_meta_leave_one_out.md),
[`epidist_model_prior.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_model_prior.epidist_meta_model.md),
[`epidist_newdata.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_meta_model.md),
[`epidist_transform_data_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_meta_model.md),
[`is_epidist_meta_model()`](https://epidist.epinowcast.org/reference/is_epidist_meta_model.md),
[`new_epidist_meta_model()`](https://epidist.epinowcast.org/reference/new_epidist_meta_model.md)
