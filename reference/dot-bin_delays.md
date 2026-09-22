# Bin the delays of each stratum

Bin the delays of each stratum

## Usage

``` r
.bin_delays(delays, binwidth = 1, keep = NULL)
```

## Arguments

- delays:

  A `tibble` of delays with their `.stratum`, as returned by
  [`.draw_strata()`](https://epidist.epinowcast.org/reference/dot-draw_strata.md).

- binwidth:

  The width of the delay bins, on the scale of the event times. Defaults
  to 1, the daily censoring the data usually has.

- keep:

  A character vector of columns of `delays` to keep, which must be
  constant within a stratum.

## Value

A `tibble` with one row per stratum and bin, holding the lower edge of
the bin as `delay`, the weighted count `n`, the proportion of the
stratum `p` and the density `density`.
