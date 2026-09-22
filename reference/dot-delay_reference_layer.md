# Draw a reference delay distribution over the observed delays

Draw a reference delay distribution over the observed delays

## Usage

``` r
.delay_reference_layer(reference, family, plot_data, binwidth)
```

## Arguments

- reference:

  A named numeric vector of the distributional parameters of a delay
  distribution to draw over the columns, as `c(mu = 1.8, sigma = 0.5)`
  for a lognormal delay. If `NULL`, the default, no distribution is
  drawn.

- family:

  A `brms` family, or the name of one, giving the delay distribution
  `reference` holds the parameters of. Defaults to `"lognormal"`.

- plot_data:

  The binned delays, as returned by
  [`.bin_delays()`](https://epidist.epinowcast.org/reference/dot-bin_delays.md).

- binwidth:

  The width of the delay bins, on the scale of the event times. Defaults
  to 1, the daily censoring the data usually has.

## Value

A `ggplot2` layer.
