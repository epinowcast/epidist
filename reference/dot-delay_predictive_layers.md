# Draw the predicted delays over the observed delays

Draw the predicted delays over the observed delays

## Usage

``` r
.delay_predictive_layers(predicted, single)
```

## Arguments

- predicted:

  The binned predicted delays, as returned by
  [`.predicted_delays()`](https://epidist.epinowcast.org/reference/dot-predicted_delays.md).

- single:

  A logical, `TRUE` when there is a single stratum.

## Value

A list of `ggplot2` layers.
