# Create an `epidist_estimates_data` object from a data frame

This method takes a `data.frame` of published summary estimates and
creates an `epidist_estimates_data` object. Column names may either
match the names used below or be supplied via the corresponding
argument.

## Usage

``` r
# S3 method for class 'data.frame'
as_epidist_estimates_data(
  data,
  study = NULL,
  type = NULL,
  value = NULL,
  se = NULL,
  n = NULL,
  p = NULL,
  pwindow = NULL,
  swindow = NULL,
  relative_obs_time = NULL,
  trunc_adjusted = NULL,
  trunc_design = NULL,
  cens_adjusted = NULL,
  delay_min = NULL,
  growth_rate = NULL,
  max_delay = NULL,
  advise = TRUE,
  ...
)
```

## Arguments

- data:

  A `data.frame` of published summary estimates.

- study:

  A string giving the column of `data` containing the study identifier.
  Defaults to `NULL` which assumes the variable `study` is present.

- type:

  A string giving the column of `data` containing the summary type. Each
  entry must be one of `"mean"`, `"sd"`, or `"quantile"`. Defaults to
  `NULL` which assumes the variable `type` is present.

- value:

  A string giving the column of `data` containing the reported value of
  the summary. Defaults to `NULL` which assumes the variable `value` is
  present.

- se:

  A string giving the column of `data` containing the reported standard
  error of the summary. Optional. When supplied it overrides the
  standard error implied by the sample size, and takes the row out of
  the joint likelihood
  [`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md)
  otherwise uses for summaries a study computed from the same delays. It
  is always on the scale of the reported `value`, so for a `"quantile"`
  row it is a standard error for the reported delay.

- n:

  A string giving the column of `data` containing the number of delays
  the summary was computed from. Required unless `se` is supplied.

- p:

  A string giving the column of `data` containing the probability of a
  reported quantile. Required for rows with `type` of `"quantile"` and
  ignored otherwise.

- pwindow, swindow:

  Strings giving the columns of `data` containing the primary and
  secondary event censoring window widths. Default to 1 (daily
  reporting) when not supplied. A fully adjusted study (`cens_adjusted`
  code 1) does not use them, because its estimand is the continuous
  delay distribution itself, so its rows may leave them `NA`. Every
  other code needs them.

- relative_obs_time:

  A string giving the column of `data` containing the observation time
  relative to the primary event, that is the right truncation point on
  the delay scale for a cohort design, or the length of the collection
  window for an accrual design. Defaults to `Inf`, meaning no
  truncation.

- trunc_adjusted:

  A string giving the column of `data` containing a logical flag for
  whether the study corrected for right truncation. Defaults to `TRUE`
  where no `relative_obs_time` is supplied and `FALSE` otherwise. A
  study assumed to have adjusted is warned about, because real time
  estimates are right truncated unless the study corrected for it and
  reviews rarely record which studies did. Supply the column to say so
  yourself.

- trunc_design:

  A string giving the column of `data` containing how the study stopped
  collecting data, either `"cohort"` (it followed every primary event
  for the same `relative_obs_time`) or `"accrual"` (it collected over a
  window of that length and stopped at its calendar end). Defaults to
  `"cohort"`, and is only used for studies that did not adjust for right
  truncation. The accrual weight on the grid of a study that did not
  adjust for censoring is exact whenever `relative_obs_time` is a
  multiple of `pwindow`, for any `swindow`. The weight used for the
  uniform single interval approximation is exact only for a narrow
  `pwindow`, and puts the implied mean about 3% high with a weekly
  primary window, a collection window of 28 days, a delay of mean 4.6
  days and a growth rate of 0.2. See
  [`vignette("model")`](https://epidist.epinowcast.org/articles/model.md).

- cens_adjusted:

  A string giving the column of `data` containing the censoring
  adjustment code (`0`, `1`, `2`, `3`, or `4`, as described above).
  Defaults to 0.

- delay_min:

  A string giving the column of `data` containing the smallest delay the
  study counted, its left truncation point, on the scale the study
  reported. Defaults to 0, meaning the study counted every delay, which
  for `cens_adjusted` code 4 includes any delay reported below zero.
  Must be below the grid cutoff, and no reported mean or quantile may
  fall below it.

- growth_rate:

  A string giving the column of `data` containing the exponential growth
  rate of primary events during the study period. Defaults to 0. It
  plays two roles. Within each primary window it tilts the primary event
  towards the end of the window, which for a daily window is negligible.
  Under `trunc_design = "accrual"` it also weights the follow up
  available to each delay, which is the dynamical bias of a growing
  epidemic and can move the implied mean by a day or more. A non-zero
  rate is expensive, because the primary censored delay distribution
  then has no analytical solution and every evaluation becomes a
  numerical integration. Leave it at 0 unless the study accrued cases
  over a period of growth. It is a known quantity here, taken from the
  study. For individual level data the same rate is estimated instead,
  as the `pgrowth` parameter of `primary = "expgrowth"` in
  [`as_epidist_marginal_model()`](https://epidist.epinowcast.org/reference/as_epidist_marginal_model.md).
  See
  [`vignette("primary-events")`](https://epidist.epinowcast.org/articles/primary-events.md).

- max_delay:

  A string giving the column of `data` containing the delay beyond which
  the implied summaries are truncated when building the discrete grid.
  Only used when the study adjusted for right truncation. Defaults to
  the delay beyond which one percent of the second moment of a lognormal
  matched to the study's summaries lies, through its mean and standard
  deviation, or its median and largest quantile above the median where
  it reported only quantiles. The lognormal is used whatever family is
  fitted later, because the family is not known when the data are built
  and, for the same mean and standard deviation, its tail is heavier
  than the gamma's or the Weibull's, so the cutoff is far enough for
  those families and only longer than they need. That is the yardstick
  of the short cutoff check in the Checks section, so the default never
  trips it where it binds. It is rounded up to a whole number of
  secondary windows, with a minimum of ten and a maximum of twenty times
  the largest reported value, because for a heavy tail one percent of
  the second moment lies thousands of delays out, and is five times the
  largest reported value where nothing can be matched. Raise it for a
  delay with a longer tail than a lognormal, whose implied standard
  deviation is biased downwards if the distribution has not decayed by
  the cutoff, and lower it to fit faster. A message names the studies
  whose cutoff is too short, see the Checks section.

- advise:

  Whether to run the advisory checks of the Checks section and message
  about the studies they flag. Defaults to `TRUE`. The list method sets
  it to `FALSE` for each element and runs the checks once on the
  combined object.

- ...:

  Not used in this method.

## What we need from each study

Published delay estimates are almost never estimates of the true
continuous delay distribution. To use them we forward model what the
study's estimation procedure would converge to, which means we need to
know how the study handled the biases we support, along with the data
process it saw.
[`vignette("model")`](https://epidist.epinowcast.org/articles/model.md)
derives each estimand, and
[`vignette("meta")`](https://epidist.epinowcast.org/articles/meta.md)
works through assembling this metadata for simulated and real studies.
For each study we need:

- **How it adjusted for censoring** (`cens_adjusted`). A taxonomy of the
  common ways interval censored delays are summarised, deliberately
  limited to a few widely used approaches:

  - `0`: no adjustment. The study took integer date differences (for
    example date of onset subtracted from date of report) and summarised
    them directly. This is the most common case in the literature.

  - `1`: fully adjusted. The study used a method targeting the
    underlying continuous distribution, such as a double interval
    censored likelihood.

  - `2`: uniform single interval approximation. The study adjusted the
    secondary interval only, assuming a uniform delay within it, and
    left the primary interval uncorrected.

  - `3`: midpoint imputation. The study assigned each delay to the
    centre of the interval it was observed in, which shifts every
    reported delay up by half a secondary window without changing its
    spread.

  - `4`: midpoint imputation with a uniform interval. The study placed
    the primary event at the midpoint of its window and integrated the
    secondary interval. Common where the primary event has a wide
    exposure window and the secondary date is recorded precisely. With a
    wide primary window the shortest delays are reported below zero, so
    the estimand puts mass there. Set `delay_min` if the study dropped
    them.

  Use code `3` for a study that midpointed the secondary interval and
  left the primary alone. Anything more exotic must be approximated by
  whichever code is closest, and if you cannot tell which a study used,
  state the assumption you are making.

- **Whether it adjusted for right truncation** (`trunc_adjusted`) and,
  if it did not, **the observation time** (`relative_obs_time`) and
  **how collection stopped** (`trunc_design`). For a cohort the
  observation time is the truncation point on the delay scale. For an
  accrual design, where collection stopped at a calendar date, it is the
  length of the collection window, which is usually easier to read off a
  paper. Real time estimates are right truncated unless the study
  corrected for it.

- **The censoring windows** (`pwindow`, `swindow`). The width, in the
  same time units as the delay, of the interval each event was observed
  in. Daily reporting gives windows of 1, weekly reporting gives 7. A
  fully adjusted study does not use them and may leave them `NA`.

- **The sample size** (`n`), the number of delays the summary was
  computed from. This sets the sampling uncertainty on the reported
  value. A reported standard error (`se`) may be given instead, and
  takes precedence when supplied.

- **The minimum delay it counted** (`delay_min`), where the study
  dropped delays below some point. Its summaries then describe a left
  truncated delay distribution. Defaults to 0, meaning the study counted
  every delay.

Systematic reviews rarely record this metadata, so you must supply your
own assumption and say so alongside any results. Where it is missing
entirely, a covariate for the phase of the outbreak each estimate was
made in is another option, since the `brms` formula makes it a
meta-regression that estimates the residual bias rather than correcting
it mechanically.

## What shape a study reported its estimate in

This method takes a long table with one row per reported summary. Two
other shapes are common enough to have their own entry points, each
returning an object this one would.

- [`epidist_estimates_summaries()`](https://epidist.epinowcast.org/reference/epidist_estimates_summaries.md)
  takes one study's summaries in wide form.

- [`epidist_estimates_parameters()`](https://epidist.epinowcast.org/reference/epidist_estimates_parameters.md)
  takes the parameters of a distribution a study fitted, which studies
  often publish in place of summaries, and converts them to the
  summaries the fitted distribution implies. Reported parameters are not
  a `type` here, because the family a study fitted need not be the
  family being fitted to it.

A study that published draws of its parameters, rather than point
summaries of them, can report the correlation between the quantities it
reports. Pass the draws to
[`as_epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.md)
and the result to
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md),
which is the only route by which a covariance reaches
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md).
Such rows need no `n` and no `se`.

Contributions from several studies combine by passing them in a list,
which
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
binds into one object.

## Checks

Beyond validating its input, this method runs advisory checks on the
summaries and messages about the studies they flag. They run once, when
the object is built, and not again when it is passed to
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md).
Each message names the studies concerned, and the row of the input where
a single summary is meant, and a pointer here follows the messages of a
call. Studies combined in a list are checked once, together, and
[`simulate_study()`](https://epidist.epinowcast.org/reference/simulate_study.md)
leaves the checks to that combined object. Set `advise = FALSE` to skip
them.

- **Assumed truncation adjustment.** Where no `trunc_adjusted` column is
  supplied, a study with no finite `relative_obs_time` is taken to have
  adjusted for right truncation and every other study not to have. Real
  time estimates are right truncated unless the study corrected for it,
  and reviews rarely record which did, so this is the assumption most
  likely to be wrong and is a warning rather than a message.

- **Short grid cutoff.** The implied summaries of a study that adjusted
  for right truncation but is evaluated on a grid, which is a study with
  `cens_adjusted` 0 or 3, or 2 or 4 with a non zero `growth_rate`, run
  to `max_delay`. A cutoff the delay distribution has not decayed by
  biases them downwards, and the standard deviation most, because the
  tail beyond the cutoff carries a share of the second moment out of all
  proportion to its mass. A lognormal is matched to what the study
  reported, through its mean and standard deviation, or its median and
  largest quantile above the median where it reported only quantiles,
  and the study is flagged when more than 2% of the second moment of
  that lognormal lies beyond the cutoff. That is where the standard
  deviation on the grid falls about 1% short, and the shortfall grows
  with the share. Studies reporting neither pair are not checked. Raise
  `max_delay` for the study.

- **Coarse quadrature.** The moments and distribution function of a
  continuous estimand truncated at the grid cutoff are computed by
  Simpson's rule on equally spaced intervals from `delay_min` to the
  cutoff, so the node spacing is set by the cutoff and not by the scale
  of the delay. The number of intervals is chosen per study so that the
  spacing is at most a quarter of the spread the study reported, with
  `options(epidist.meta_n_quad)` as its floor and 2000 as a cap the
  option lifts when set above it. A study whose cutoff is very long
  relative to its spread hits the cap and is left with nodes further
  apart than that, so its implied summaries may be inaccurate. This
  covers a study that did not adjust for right truncation and used a
  continuous adjustment (`cens_adjusted` 1, 2 or 4), a study that did
  adjust but whose primary events were not uniform within their window
  (`cens_adjusted` 2 or 4 with a non zero `growth_rate`), and the
  quantiles of a study reporting a covariance matrix, which are read off
  the same nodes. Raise the option above the cap before building the
  model data, or lower `max_delay`.

- **Coarse quantiles.** A study that summarised interval censored delays
  without adjusting for censoring (`cens_adjusted` 0 or 3) reports
  quantiles of a discrete distribution, which the model interpolates
  through the mid points of its cells. The reported value is still
  rounded to that grid, and what the interpolation leaves behind does
  not shrink with the study sample size. It is a few percent once a
  reported quantile sits a few tens of cells above the smallest delay
  the study counted, and tens of percent when it sits within about ten.
  A study is flagged on its smallest reported quantile, the one nearest
  that edge of the grid, because the residual on that quantile is what
  biases the fitted spread even when the larger quantiles of the same
  study sit well up the grid. Check that `swindow` is the resolution the
  study worked at. A reported mean and standard deviation of the same
  delays do not carry this residual, so fit them in preference where the
  study gives them.

- **Several integer day quantiles from a large study.** A quantile of
  delays counted in whole censoring windows is a discrete statistic, and
  the information it carries about the delay distribution saturates once
  the binomial spread of the crossing point of the empirical
  distribution function is narrower than a window. A single such
  quantile is fitted as the exact crossing event, but several are still
  fitted with the multinomial on the continuity corrected distribution
  function, whose claimed precision keeps growing with the sample size.
  It is calibrated at around thirty delays and overconfident from around
  a hundred, so a study reporting two or more such quantiles from more
  than 100 delays is flagged and will be weighted too heavily. Fit a
  reported mean and standard deviation instead where one is available.

- **Heavy tailed standard deviation.** The sampling standard error of a
  reported standard deviation is \\\sigma \sqrt{(\kappa - 1) / (4 n)}\\,
  with \\\kappa\\ the kurtosis of the delays. The normal approximation
  behind it holds while that relative standard error is below about a
  quarter. Above it the sampling distribution of a sample standard
  deviation is far from normal, the asymptotic standard error overstates
  its spread by up to two times, and the joint likelihood of a mean and
  standard deviation pair is biased by about a standard error. The
  kurtosis is taken from the reported mean and standard deviation under
  a lognormal delay, which is a plausible tail for a delay of that
  coefficient of variation. Where the study reports quantiles inside the
  body of the distribution, those are safer to fit.

## See also

Other estimates_data:
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md),
[`as_epidist_estimates_data.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.epidist_estimates_data.md),
[`as_epidist_estimates_data.epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.epidist_multivariate.md),
[`as_epidist_estimates_data.list()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.list.md),
[`assert_epidist.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_estimates_data.md),
[`epidist_estimates_parameters()`](https://epidist.epinowcast.org/reference/epidist_estimates_parameters.md),
[`epidist_estimates_summaries()`](https://epidist.epinowcast.org/reference/epidist_estimates_summaries.md),
[`is_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/is_epidist_estimates_data.md),
[`new_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/new_epidist_estimates_data.md)

## Examples

``` r
as_epidist_estimates_data(
  data.frame(
    study = c("A", "A", "B"),
    type = c("mean", "sd", "quantile"),
    value = c(7.5, 3.6, 11.2),
    p = c(NA, NA, 0.9),
    n = c(120, 120, 80),
    relative_obs_time = c(20, 20, Inf),
    trunc_adjusted = c(FALSE, FALSE, TRUE),
    cens_adjusted = c(0, 0, 1)
  )
)
#> ℹ No `pwindow` column supplied, assuming a censoring window of 1 (daily
#>   reporting) for every study.
#> ℹ No `swindow` column supplied, assuming a censoring window of 1 (daily
#>   reporting) for every study.
#> ℹ No trunc_design column supplied, assuming every study that did not adjust for
#>   right truncation followed a cohort with a common observation time rather than
#>   accruing primary events up to a calendar collection stop.
#> ℹ No max_delay column supplied, using the delay beyond which 1% of the second
#>   moment of a lognormal matched to each study's summaries lies (at least 10 and
#>   at most twenty times the largest reported value, in whole secondary windows)
#>   as the grid cutoff, or five times the largest reported value where nothing
#>   can be matched. Raise it if the delay has a longer tail than that, and lower
#>   it to speed up fitting.
#> # A tibble: 3 × 16
#>   study type     value    se     n     p pwindow swindow relative_obs_time
#>   <chr> <chr>    <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>             <dbl>
#> 1 A     mean       7.5    NA   120  NA         1       1                20
#> 2 A     sd         3.6    NA   120  NA         1       1                20
#> 3 B     quantile  11.2    NA    80   0.9       1       1               Inf
#> # ℹ 7 more variables: trunc_adjusted <lgl>, trunc_design <chr>,
#> #   cens_adjusted <int>, delay_min <dbl>, growth_rate <dbl>, max_delay <dbl>,
#> #   mvn_id <chr>
```
