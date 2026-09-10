# Which summary estimates have a tilted primary event

The primary event is uniform within its window only for a known growth
rate of zero. A non zero rate tilts it, and an estimated rate is taken
to tilt it whatever value the parameter holds, so that the choice of
path does not depend on a parameter.

## Usage

``` r
.estimates_growth_tilted(data)
```

## Arguments

- data:

  An `epidist_estimates_data` object, or a data frame with its
  `growth_rate` and `growth_rate_sd` columns.

## Value

A logical vector, one entry per row.
