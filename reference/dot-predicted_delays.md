# Bin the delays a fitted model predicts for each case

Predicts the delay of every case in the model data from each posterior
draw, bins the predictions of each draw as
[`.bin_delays()`](https://epidist.epinowcast.org/reference/dot-bin_delays.md)
bins the observed delays, and summarises the proportion in each bin
across the draws. Draws with no prediction in a bin count as a
proportion of zero.

## Usage

``` r
.predicted_delays(x, delays, binwidth = 1, ndraws = 100, probs = c(0.05, 0.95))
```

## Arguments

- x:

  An `epidist_linelist_data` or `epidist_aggregate_data` object, a named
  list of them to compare, or a model fitted with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

- delays:

  A `tibble` of the observed delays with their `.stratum`, as returned
  by
  [`.draw_strata()`](https://epidist.epinowcast.org/reference/dot-draw_strata.md).

- binwidth:

  The width of the delay bins, on the scale of the event times. Defaults
  to 1, the daily censoring the data usually has.

- ndraws:

  The number of posterior draws to predict from, sampled at random.
  Defaults to 100, which is enough for the median and the quantiles of a
  binned distribution and bounds the size of the prediction. Use `NULL`
  to predict from every draw.

- probs:

  A numeric vector of two probabilities giving the quantiles the ribbon
  spans. Defaults to `c(0.05, 0.95)`.

## Value

A `tibble` with one row per stratum and bin, holding the lower edge of
the bin as `delay`, the posterior median `density` and the `lower` and
`upper` quantiles of it.
