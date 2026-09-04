# Extract the meta model slots for a single row

Group rows point into the flat member arrays passed to Stan as data, so
the reported values and cumulative counts of the group are read back out
here for the R mirrors of the joint likelihoods.

## Usage

``` r
.meta_row_slots(i, prep)
```

## Arguments

- i:

  The row index.

- prep:

  A `brms` prep object.

## Value

A named list of the observation type, study metadata and reported values
for row `i`.
