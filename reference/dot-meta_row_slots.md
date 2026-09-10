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

## Details

A row whose `growth_known` slot is 0 estimates its growth rate as the
`pgrowth` distributional parameter, so its `growth_rate` element holds
one value per posterior draw rather than the number in the slot.
Everything that works one draw at a time takes the slots of that draw
from
[`.meta_draw_slots()`](https://epidist.epinowcast.org/reference/dot-meta_draw_slots.md).
A fit made before the slot existed has no `growth_known` and every row
of it is known.
