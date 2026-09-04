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
gives the forward model and the sampling likelihoods, and
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
is fitted alone. One observation is therefore a group rather than a
single reported value, so `log_lik()` and
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html) report per
group, and `loo` only compares fits to the same studies and the same mix
of individual and summary rows. See
[`vignette("faq")`](https://epidist.epinowcast.org/articles/faq.md).

Three consequences of the sampling likelihoods change what you should
do.

- The standard errors are plug in quantities that depend on the
  parameters, so studies no single distribution can explain may be
  accommodated by inflating the implied standard deviation rather than
  by moving the location, and sampling can become multimodal. Allow for
  genuine differences with a term such as `mu ~ 1 + (1 | study)` rather
  than relying on the sampling error alone.

- Quantiles read off a fitted distribution rather than the empirical
  data have smaller sampling error than assumed here. Supply a reported
  `se` in
  [`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
  for those rows, which also takes them out of the joint quantile
  likelihood.

- The normal approximations degrade at small study sample sizes, and
  summaries of different kinds from one study, such as a mean and a
  median, are treated as independent. A study that published draws of
  its parameters avoids the second, because
  [`as_epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.md)
  turns them into a covariance over the summaries that is fitted
  jointly.

Two approximations are worth knowing about before fitting quantiles.

- A study that took integer date differences reports quantiles of a
  discrete distribution. The model interpolates its grid distribution
  function through the mid points of the cells, but the reported value
  is itself rounded to that grid, and what is left does not shrink with
  the study sample size. It stays under 4% on the mean and 9% on the
  standard deviation once the reported quantiles sit twenty five or more
  cells above the smallest delay the study counted, and reaches tens of
  percent on both when they sit within ten. Refitting the median and
  interquartile range of a lognormal delay of mean 5.9 days, on daily
  windows with an observation time of 12 days, recovers a delay mean 27%
  high and a standard deviation 69% high. The same study's mean and
  standard deviation recover the truth, so prefer those where a study
  reports them, and check that `swindow` is the resolution it worked at.
  [`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
  warns for studies in this range.

- The accrual weight applied to a study that stopped collecting at a
  calendar date treats the last primary window as complete, so it is
  exact on the grid only when `relative_obs_time` is a multiple of
  `pwindow`, and it averages over the primary window for a study that
  adjusted the secondary interval only. With a weekly primary window, a
  collection window of 28 days and a delay of mean 4.6 days the latter
  puts the implied mean 0.8% high at a growth rate of 0.05 and 2.6% high
  at 0.2.
  [`vignette("model")`](https://epidist.epinowcast.org/articles/model.md)
  gives the measurements.

Two settings trade accuracy against speed: `max_delay` in
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md),
which sets the grid a study that adjusted for right truncation is
summarised on and needs raising for a long tailed delay, and
`options(epidist.meta_n_quad = )`, the smallest number of quadrature
intervals used where a study is summarised by quadrature instead. Each
study is given as many intervals as it needs to resolve the spread it
reported, up to a cap of 2000 that the option lifts when set above it,
and the number is held in the `n_quad` column of the model data.

## See also

Other meta_model:
[`as_epidist_meta_model.NULL()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.NULL.md),
[`as_epidist_meta_model.epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_aggregate_data.md),
[`as_epidist_meta_model.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_estimates_data.md),
[`as_epidist_meta_model.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.epidist_linelist_data.md),
[`assert_epidist.epidist_meta_model()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_meta_model.md),
[`epidist_family_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.epidist_meta_model.md),
[`epidist_formula_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_meta_model.md),
[`epidist_model_prior.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_model_prior.epidist_meta_model.md),
[`epidist_newdata.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_newdata.epidist_meta_model.md),
[`epidist_transform_data_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_transform_data_model.epidist_meta_model.md),
[`is_epidist_meta_model()`](https://epidist.epinowcast.org/reference/is_epidist_meta_model.md),
[`new_epidist_meta_model()`](https://epidist.epinowcast.org/reference/new_epidist_meta_model.md)
