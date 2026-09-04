# The range of delays a study's reported distribution describes

A study that fitted a distribution to right truncated data without
correcting for the truncation reported a distribution with more spread
than the delays it saw, because its family carries a tail beyond the
point its data stop. Summaries of the reported distribution are
therefore taken over the range of delays the study could have seen,
which is the same range the meta model computes its implied summaries
over. The metadata that gives that range is already on its way onto the
returned rows, so it is read from there rather than asked for again.

## Usage

``` r
.estimates_reported_support(...)
```

## Arguments

- ...:

  The metadata columns passed to
  [`epidist_estimates_parameters()`](https://epidist.epinowcast.org/reference/epidist_estimates_parameters.md).

## Value

A list with the study's `lower` and `cutoff` delays.
