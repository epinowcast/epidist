# Report studies from an `epireview` parameter table

Maps a table of published delay estimates from the
[epireview](https://mrc-ide.github.io/epireview/) package, one row per
reported estimate, to the long format
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
takes, and returns the `epidist_estimates_data` object it builds.
`epireview` collates the parameter estimates gathered by the Pathogen
Epidemiology Review Group. It is not on CRAN, and is installed from the
[mrc-ide r-universe](https://mrc-ide.r-universe.dev). Filter the
`params` table of
[`epireview::load_epidata()`](https://mrc-ide.github.io/epireview/reference/load_epidata.html)
to one delay before passing it here, because the rows of one object all
describe the same delay.

## Usage

``` r
epidist_estimates_epireview(
  data,
  study = "article_label",
  metadata = NULL,
  keep = NULL,
  advise = TRUE,
  ...
)
```

## Arguments

- data:

  A `data.frame` of `epireview` delay estimates, such as the `params`
  element of
  [`epireview::load_epidata()`](https://mrc-ide.github.io/epireview/reference/load_epidata.html)
  filtered to one delay. It needs the `parameter_value`,
  `parameter_value_type` and `population_sample_size` columns and the
  column named by `study`. The other `parameter_*` and `distribution_*`
  columns are used where present.

- study:

  A string naming the column of `data` that identifies the study.
  Defaults to `"article_label"`.

- metadata:

  A `data.frame` of study metadata, or `NULL`. It needs a `study` column
  holding values of the `study` column of `data`, and takes any of `n`,
  `pwindow`, `swindow`, `relative_obs_time`, `trunc_adjusted`,
  `trunc_design`, `cens_adjusted`, `delay_min` and `growth_rate`, as
  documented in
  [`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md).
  Each value replaces the value of that column for the study's records,
  and an `NA` leaves it as it is.

- keep:

  A character vector of columns of `data` to carry onto the rows of the
  result, for use as covariates. Defaults to `NULL`.

- advise:

  Whether to run the advisory checks of
  [`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md).
  Defaults to `TRUE`.

- ...:

  Study metadata applied to every record, each a single value, as
  documented in
  [`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md).
  Any of `pwindow`, `swindow`, `relative_obs_time`, `trunc_adjusted`,
  `trunc_design`, `cens_adjusted`, `delay_min` and `growth_rate`.

## Value

An `epidist_estimates_data` object.

## What each record becomes

A record is mapped by its `parameter_value_type`.

- A `"Mean"` becomes a `"mean"` row. A `"Standard Deviation"` in
  `parameter_uncertainty_singe_type` becomes a matching `"sd"` row, as
  does the second parameter of a fitted distribution that `epireview`
  records as a mean and a standard deviation. A `"Standard Error"`
  becomes the `se` of the mean row instead, and so does a 95% confidence
  or credible interval of the mean, as its width over
  `2 * qnorm(0.975)`.

- A `"Median"` becomes a `"quantile"` row at `p = 0.5`, with further
  rows at `p = 0.25` and `p = 0.75` where `parameter_uncertainty_type`
  is `"IQR"`.

- A record whose `distribution_type` is a gamma, a Weibull or a
  lognormal reported by its natural parameters (a shape with a scale or
  a rate, or a meanlog with a sdlog) becomes the mean and standard
  deviation the fitted distribution implies over the delays the study
  could have seen, as
  [`epidist_estimates_parameters()`](https://epidist.epinowcast.org/reference/epidist_estimates_parameters.md)
  computes them. Its reported value and spread are not used again,
  because they are functions of the same parameters. `epireview` records
  no uncertainty for the parameters, so the rows take their sampling
  uncertainty from the sample size.

A range is the smallest and largest delay a study saw rather than a
summary of the distribution, and is not used. A spread that does not
match the value type, such as a standard deviation reported alongside a
median, is not used either, because the two kinds of summary from a
study that reported integer date differences are fitted as though they
were independent.

Records that cannot be mapped are dropped with a message naming them.
These are records reporting an inverse rate, units other than days, a
value with a scaling exponent, no mean or median and no usable
distribution parameters, and records left with no sample size and no
standard error. Fill a missing sample size through `metadata`.

Several records may share a study, for example estimates stratified by
outbreak or by group, and they are then fitted as separate summaries of
the same study.

## Study metadata

`epireview` does not record how a study handled censoring or right
truncation, its censoring windows or its observation time. Give the
metadata that applies to every record through `...`, and the metadata
that differs by study through `metadata`, a data frame with one row per
study whose values replace those of `...` for that study's records.
Anything given for neither is assumed as
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
assumes it, with the same messages, and a study that `metadata` leaves
blank for a column it holds is assumed in the same way and named. The
Checks section of
[`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md)
says which assumptions matter most.

The result is an `epidist_estimates_data` object, so metadata learnt
later can be edited in with
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
or `[<-`. The object is checked again after each change and drops its
class if the change breaks a requirement, so make related changes, such
as setting `trunc_adjusted` to `FALSE` and giving the
`relative_obs_time` it then needs, in one call. See
[epidist_data](https://epidist.epinowcast.org/reference/epidist_data.md).

## See also

Other estimates_data:
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md),
[`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md),
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
# A table with the columns epireview uses
records <- data.frame(
  article_label = c("A 2015", "B 2016", "B 2016"),
  parameter_value = c(10.6, 14, 8.9),
  parameter_value_type = c("Mean", "Median", "Mean"),
  parameter_unit = "Days",
  parameter_uncertainty_single_value = c(3.2, NA, NA),
  parameter_uncertainty_singe_type = c("Standard Deviation", NA, NA),
  parameter_uncertainty_lower_value = c(NA, 11, NA),
  parameter_uncertainty_upper_value = c(NA, 15, NA),
  parameter_uncertainty_type = c(NA, "IQR", NA),
  population_sample_size = c(76, 20, 92)
)
epidist_estimates_epireview(
  records,
  metadata = data.frame(
    study = "A 2015", relative_obs_time = 60, trunc_adjusted = FALSE
  ),
  trunc_adjusted = TRUE,
  cens_adjusted = 0
)
#> ℹ No relative_obs_time given for "B 2016", assuming Inf as for a study with no
#>   relative_obs_time column.
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
#> # A tibble: 6 × 17
#>   study  type     value    se     n     p pwindow swindow relative_obs_time
#>   <chr>  <chr>    <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>             <dbl>
#> 1 A 2015 mean      10.6    NA    76 NA          1       1                60
#> 2 A 2015 sd         3.2    NA    76 NA          1       1                60
#> 3 B 2016 quantile  14      NA    20  0.5        1       1               Inf
#> 4 B 2016 quantile  11      NA    20  0.25       1       1               Inf
#> 5 B 2016 quantile  15      NA    20  0.75       1       1               Inf
#> 6 B 2016 mean       8.9    NA    92 NA          1       1               Inf
#> # ℹ 8 more variables: trunc_adjusted <lgl>, trunc_design <chr>,
#> #   cens_adjusted <int>, delay_min <dbl>, growth_rate <dbl>,
#> #   growth_rate_sd <dbl>, max_delay <dbl>, mvn_id <chr>
ebola <- suppressMessages(epireview::load_epidata("ebola"))$params
onset_to_death <- ebola[
  ebola$parameter_type_short == "delay_onset_to_death",
]
epidist_estimates_epireview(
  onset_to_death,
  trunc_adjusted = TRUE,
  cens_adjusted = 0,
  keep = "method_moment_value"
)
#> ℹ Dropped 16 records that cannot be mapped to a summary of the delay
#>   distribution:
#> • "Yan 2015" (row 1), "Senga 2016 (2)" (row 8), "Do 2016" (row 30) report no
#>   sample size and no standard error.
#> • "Siettos 2016" (row 4), "Siettos 2015 (1)" (row 5), "Siettos 2015 (2)" (row
#>   6), "Rivers 2014 (1)" (row 11), "Rivers 2014 (2)" (row 12), "Roels 1999" (row
#>   21), "Kucharski 2015" (row 22), "Francesconi 2003 (1)" (row 26), "Agua-Agum
#>   2015 (1)" (row 34), "Agua-Agum 2015 (2)" (row 35), "Agua-Agum 2015 (3)" (row
#>   36), "Sadek 1999 (b)" (row 40), "Diaz 2018" (row 41) report no mean or median
#>   and no usable distribution parameters.
#> ℹ No `pwindow` column supplied, assuming a censoring window of 1 (daily
#>   reporting) for every study.
#> ℹ No `swindow` column supplied, assuming a censoring window of 1 (daily
#>   reporting) for every study.
#> ℹ No relative_obs_time column supplied, assuming no observation time limit (no
#>   right truncation) for every study.
#> ℹ No max_delay column supplied, using the delay beyond which 1% of the second
#>   moment of a lognormal matched to each study's summaries lies (at least 10 and
#>   at most twenty times the largest reported value, in whole secondary windows)
#>   as the grid cutoff, or five times the largest reported value where nothing
#>   can be matched. Raise it if the delay has a longer tail than that, and lower
#>   it to speed up fitting.
#> ! "Nanclares 2016" (row 7), "Li 2016 (a)" (row 14), "Khan 1999" (row 27),
#>   "Camacho 2014" (row 35), "Baron 1983" (row 36), "Bah 2015" (row 38),
#>   "Miglietta 2019" (row 43), and "Nsio 2019" (row 44) report quantiles less
#>   than ten days above their smallest counted delay. Quantiles rounded to whole
#>   days that close to the origin carry a bias that a larger sample does not
#>   shrink.
#> ! "Muoghalu 2017" (row 13), "Rosello 2015 (2)" (row 18), "Maganga 2014" (row
#>   24), and "Folarin 2016" (row 30) report a standard deviation whose relative
#>   standard error is above 0.25 under the lognormal tail their mean implies. A
#>   sampling error that large is far from normal, so the likelihood of the
#>   standard deviation cannot be trusted.
#> ℹ See the Checks section of `?as_epidist_estimates_data`.
#> # A tibble: 45 × 18
#>    study         type  value    se     n     p pwindow swindow relative_obs_time
#>    <chr>         <chr> <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl>             <dbl>
#>  1 Xu 2016       mean   8.6   NA      76 NA          1       1               Inf
#>  2 Xu 2016       sd     4.8   NA      76 NA          1       1               Inf
#>  3 Uyeki 2016    quan… 14     NA       5  0.5        1       1               Inf
#>  4 Senga 2016 (… mean  11.1   NA      92 NA          1       1               Inf
#>  5 Schieffelin … mean   9.79   0.7    38 NA          1       1               Inf
#>  6 Nanclares 20… quan…  9     NA      25  0.5        1       1               Inf
#>  7 Nanclares 20… quan…  7     NA      25  0.25       1       1               Inf
#>  8 Nanclares 20… quan… 12     NA      25  0.75       1       1               Inf
#>  9 Qureshi 2015  mean   8.9   NA      70 NA          1       1               Inf
#> 10 Qureshi 2015  sd     3.8   NA      70 NA          1       1               Inf
#> # ℹ 35 more rows
#> # ℹ 9 more variables: trunc_adjusted <lgl>, trunc_design <chr>,
#> #   cens_adjusted <int>, delay_min <dbl>, growth_rate <dbl>,
#> #   growth_rate_sd <dbl>, max_delay <dbl>, mvn_id <chr>,
#> #   method_moment_value <chr>
```
