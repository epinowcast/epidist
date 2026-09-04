# The grid cutoff implied by a set of summary estimates

Studies that did not adjust for right truncation are evaluated on a grid
running to their observation time. Studies that did are evaluated on a
grid running to `max_delay`.

## Usage

``` r
.estimates_grid_cutoff(data)
```

## Arguments

- data:

  An `epidist_estimates_data` object.

## Value

A numeric vector of grid cutoffs.
