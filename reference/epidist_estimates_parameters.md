# Report a study that published the parameters of a distribution it fitted

Studies often publish the parameters of a distribution they fitted
rather than summaries of the delays themselves. This converts those
parameters into the summaries the fitted distribution implies, which is
what
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md)
fits to.

## Usage

``` r
epidist_estimates_parameters(
  study,
  family,
  parameters,
  moments = c("mean", "sd"),
  probs = numeric(0),
  se = NULL,
  n = NULL,
  ...
)
```

## Arguments

- study:

  A string naming the study.

- family:

  The distribution the study fitted, one of `"lognormal"`, `"gamma"` or
  `"weibull"`.

- parameters:

  A named numeric vector of the reported parameters. The names must be
  `meanlog` and `sdlog` for a lognormal, `shape` with either `scale` or
  `rate` for a gamma, and `shape` and `scale` for a weibull.

- moments:

  Which moments to report, any of `"mean"` and `"sd"`.

- probs:

  A numeric vector of probabilities to report quantiles at.

- se:

  A numeric vector of the reported standard errors of `parameters`, in
  the same order. Optional.

- n:

  The number of delays the study fitted. Optional, and used for the
  sampling uncertainty of the summaries where no `se` is given.

- ...:

  Study metadata, as documented in
  [`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md).

## Value

An `epidist_estimates_data` object.

## What the reported parameters are taken to mean

The reported parameters describe the distribution the study's own
estimation procedure converged to. Where the procedure was correct, that
is the delay distribution itself. Where it was not, it is the biased
distribution the procedure targeted, which is exactly what the meta
model forward models from the study metadata. Converting to summaries
therefore covers both cases with one route, and the study metadata
documented in
[`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md)
is supplied in the same way as for any other summary.

Because the returned rows are summaries, the family the study fitted
need not match the family being fitted to it. A study reporting a gamma
can be used in a lognormal meta model. Parameters are never compared
across families, which would be meaningless. For a two parameter family
the map from parameters to a mean and a standard deviation is one to
one, so a study that fitted correctly loses nothing by being converted.

What this does assume is that summaries of the study's reported
distribution are the summaries of what its procedure targeted. That
holds exactly when the study reported a distribution of the same shape
as its estimand, and approximately otherwise, for example where a study
fitted a continuous distribution to integer date differences.

## The range the summaries are taken over

A study that did not correct for right truncation fitted a distribution
to delays that stop at its observation time, but the families studies
fit have a tail running past that point. Taking the reported
distribution's summaries over its whole support would then charge it
with spread its data never had, which is worst for a standard deviation
and can reach tens of percent for a short observation time.

Summaries are therefore taken over the range of delays the study could
have seen, conditioning the reported distribution on falling between
`delay_min` and `relative_obs_time`. This is the same range the meta
model computes its implied summaries over, so the two sides are the same
functional of the two distributions. Both fields are read from the
metadata passed through `...`, and a study that adjusted for right
truncation is left unconditioned above. A reported quantile at
probability `p` is the value the conditioned distribution puts `p`
below, so it too stays inside the observed range.

Where the study fitted a family that cannot represent its own estimand,
quantiles inside the body of the distribution are more reliable than a
standard deviation, which depends on a tail the study never saw.

## Uncertainty

Supply the standard errors the study reported on its parameters through
`se`. They are carried onto the summary scale by the delta method, using
a numerical Jacobian of the map from parameters to summaries, and the
summaries are reported with the covariance matrix that implies between
them, so they are fitted jointly as a multivariate normal in the same
way as the output of
[`as_epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.md).
For a fit with as many summaries as parameters this carries the study's
information about its parameters exactly, which fitting each summary
with its own standard error would not.

Summaries of a two parameter fit are deterministic functions of two
numbers, so at most two of them can carry the reported uncertainty, and
asking for more with `se` is an error. A study that published a full
parameter covariance rather than standard errors can draw from it, push
each draw through to the summaries, and use
[`as_epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_multivariate.md),
which needs no linearisation.

Without `se` the summaries fall back to the sample size likelihoods,
which derive their sampling uncertainty from `n`, and any number of them
may be reported.

## See also

Other estimates_data:
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md),
[`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md),
[`as_epidist_estimates_data.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.epidist_estimates_data.md),
[`as_epidist_estimates_data.epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.epidist_multivariate.md),
[`as_epidist_estimates_data.list()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.list.md),
[`assert_epidist.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_estimates_data.md),
[`epidist_estimates_summaries()`](https://epidist.epinowcast.org/reference/epidist_estimates_summaries.md),
[`is_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/is_epidist_estimates_data.md),
[`new_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/new_epidist_estimates_data.md)

## Examples

``` r
epidist_estimates_parameters(
  "study A",
  family = "gamma",
  parameters = c(shape = 4.1, rate = 0.55),
  se = c(0.4, 0.06),
  relative_obs_time = 20,
  trunc_adjusted = FALSE,
  cens_adjusted = 0
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
#> # A tibble: 2 × 16
#>   study   type  value    se     n     p pwindow swindow relative_obs_time
#>   <chr>   <chr> <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>             <dbl>
#> 1 study A mean   7.37    NA    NA    NA       1       1                20
#> 2 study A sd     3.51    NA    NA    NA       1       1                20
#> # ℹ 7 more variables: trunc_adjusted <lgl>, trunc_design <chr>,
#> #   cens_adjusted <int>, delay_min <dbl>, growth_rate <dbl>, max_delay <dbl>,
#> #   mvn_id <chr>
```
