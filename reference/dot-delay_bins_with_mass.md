# Find the bins the data or the predictions put mass in

Find the bins the data or the predictions put mass in

## Usage

``` r
.delay_bins_with_mass(observed, predicted)
```

## Arguments

- observed:

  The binned observed delays, as returned by
  [`.bin_delays()`](https://epidist.epinowcast.org/reference/dot-bin_delays.md).

- predicted:

  The binned predicted delays, as returned by
  [`.predicted_delays()`](https://epidist.epinowcast.org/reference/dot-predicted_delays.md).

## Value

A `tibble` of the `.stratum` and `delay` of each bin to draw.
