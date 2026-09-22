# Plot the observed delays of one or more datasets

Plot the observed delays of one or more datasets

## Usage

``` r
.plot_observed_delays(
  datasets,
  by = NULL,
  binwidth = 1,
  delay_min = NULL,
  reference = NULL,
  family = "lognormal"
)
```

## Arguments

- datasets:

  A named list of `epidist_linelist_data` objects.

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

## Value

A `ggplot` object.
