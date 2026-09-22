# Draw the binned delays of each stratum as columns

Draw the binned delays of each stratum as columns

## Usage

``` r
.delay_column_plot(plot_data, legend, binwidth)
```

## Arguments

- plot_data:

  The binned delays, as returned by
  [`.bin_delays()`](https://epidist.epinowcast.org/reference/dot-bin_delays.md).

- legend:

  The legend title, or `NULL` for none.

- binwidth:

  The width of the delay bins, on the scale of the event times. Defaults
  to 1, the daily censoring the data usually has.

## Value

A `ggplot` object.
