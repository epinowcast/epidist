# Plot posterior draws of the delay distribution

Plots the draws returned by
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md),
[`delay_summary_draws()`](https://epidist.epinowcast.org/reference/delay_summary_draws.md)
and
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md).
The default, `type = "parameters"`, draws the posterior density of each
distributional parameter, and of any summary column
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md)
added, in its own panel. `type = "delay"` draws the delay distribution
the draws imply over a grid of delays, as the posterior median density
with a ribbon between two quantiles, or as one line per draw when
`ndraws` is given.

## Usage

``` r
# S3 method for class 'epidist_delay_draws'
plot(x, ...)

# S3 method for class 'epidist_delay_draws'
autoplot(
  object,
  type = c("parameters", "delay"),
  by = NULL,
  pars = NULL,
  true_values = NULL,
  ndraws = NULL,
  probs = c(0.05, 0.95),
  max_delay = NULL,
  family = NULL,
  ...
)
```

## Arguments

- x, object:

  An `epidist_delay_draws` object, as returned by
  [`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md),
  [`delay_summary_draws()`](https://epidist.epinowcast.org/reference/delay_summary_draws.md)
  or
  [`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md).

- ...:

  Passed from [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  to `autoplot()`. Unused otherwise.

- type:

  Either `"parameters"`, the default, to plot the posterior density of
  each parameter, or `"delay"` to plot the delay distribution the draws
  imply.

- by:

  A character vector of columns of `object` that define the strata to
  colour by. If `NULL`, the default, the variables recorded on `object`
  are used. See the details.

- pars:

  A character vector of the columns to plot when `type = "parameters"`.
  If `NULL`, the default, the distributional parameters of the family
  are plotted, along with the `mean`, `sd` and quantile columns present.

- true_values:

  A named numeric vector of true parameter values to mark with dashed
  vertical lines when `type = "parameters"`. The names must be among the
  parameters plotted.

- ndraws:

  The number of draws per stratum to plot the delay distribution of when
  `type = "delay"`, one line each, sampled at random. If `NULL`, the
  default, the posterior median density is drawn with a ribbon between
  the `probs` quantiles instead.

- probs:

  A numeric vector of two probabilities giving the quantiles the ribbon
  spans when `type = "delay"`. Defaults to `c(0.05, 0.95)`.

- max_delay:

  The largest delay to evaluate the delay distribution at when
  `type = "delay"`. If `NULL`, the default, the posterior median of the
  99% quantile of the delay distribution is used.

- family:

  A model fit with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md), a
  `brms` family, or the name of one, giving the delay distribution. If
  `NULL`, the default, the family recorded on `object` is used.

## Value

A `ggplot` object.

## Details

The strata of the draws are coloured. By default they are the unique
combinations of the variables in the distributional parameter formulas,
which
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)
records, and each `.row` when the draws have several rows of `newdata`
but no such variables. Pass `by` to stratify by other columns of the
draws. The columns in `by` are kept in the plot data, so the plot can be
faceted by them.

The delay distribution is evaluated with the density of the family for
the lognormal, gamma and Weibull families. For any other family, delays
are simulated from each draw as in
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md)
and their density is estimated with
[`stats::density()`](https://rdrr.io/r/stats/density.html). Either way
the density is evaluated at every draw, so thin the draws with the
`ndraws` argument of
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md),
or build `newdata` with
[`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md),
when there are many.

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
`autoplot()` are the same function. Both need `ggplot2`.

The plot is drawn with
[`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
and the colour blind friendly palette the package documentation uses.
Add a theme or a scale of your own to the returned plot to override
either.

## See also

[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)
and
[`delay_summary_draws()`](https://epidist.epinowcast.org/reference/delay_summary_draws.md)
for the draws, and
[`plot_events()`](https://epidist.epinowcast.org/reference/plot_events.md)
to plot the data.

Other plot:
[`plot_events()`](https://epidist.epinowcast.org/reference/plot_events.md)

## Examples

``` r
draws <- data.frame(
  mu = rnorm(200, 1.8, 0.05),
  sigma = exp(rnorm(200, log(0.5), 0.05))
) |>
  add_summaries(family = "lognormal", probs = 0.5)
plot(draws, true_values = c(mu = 1.8, sigma = 0.5))

plot(draws, type = "delay")

plot(draws, type = "delay", ndraws = 50)
```
