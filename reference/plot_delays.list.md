# Compare the observed delays of several datasets

The names of `x` label the strata.

## Usage

``` r
# S3 method for class 'list'
plot_delays(
  x,
  by = NULL,
  binwidth = 1,
  delay_min = NULL,
  reference = NULL,
  family = "lognormal",
  ...
)
```

## Arguments

- x:

  An `epidist_linelist_data` or `epidist_aggregate_data` object, a named
  list of them to compare, or a model fitted with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

- by:

  A string naming a column of `x` to stratify the delays by.

- binwidth:

  The width of the delay bins, on the scale of the event times. Defaults
  to 1, the daily censoring the data usually has.

- delay_min:

  The minimum delay to mark with a dashed vertical line, as a number on
  the scale of the event times. If `NULL`, the default, the `delay_min`
  column of `x` is used when it has one and no line is drawn otherwise.

- reference:

  A named numeric vector of the distributional parameters of a delay
  distribution to draw over the columns, as `c(mu = 1.8, sigma = 0.5)`
  for a lognormal delay. If `NULL`, the default, no distribution is
  drawn.

- family:

  A `brms` family, or the name of one, giving the delay distribution
  `reference` holds the parameters of. Defaults to `"lognormal"`.

- ...:

  Passed to the method.

## Value

A `ggplot` object.

## See also

Other plot:
[`plot.epidist_delay_draws()`](https://epidist.epinowcast.org/reference/plot.epidist_delay_draws.md),
[`plot_delays()`](https://epidist.epinowcast.org/reference/plot_delays.md),
[`plot_delays.default()`](https://epidist.epinowcast.org/reference/plot_delays.default.md),
[`plot_delays.epidist_fit()`](https://epidist.epinowcast.org/reference/plot_delays.epidist_fit.md),
[`plot_delays.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/plot_delays.epidist_linelist_data.md),
[`plot_events()`](https://epidist.epinowcast.org/reference/plot_events.md)
