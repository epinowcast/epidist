# The slots of one meta model row for one posterior draw

A row with an estimated growth rate holds one rate per draw, see
[`.meta_row_slots()`](https://epidist.epinowcast.org/reference/dot-meta_row_slots.md),
and the implied summaries of a draw are computed from the rate of that
draw. A row with a known rate is returned as it is.

## Usage

``` r
.meta_draw_slots(slots, draw)
```

## Arguments

- slots:

  The output of
  [`.meta_row_slots()`](https://epidist.epinowcast.org/reference/dot-meta_row_slots.md).

- draw:

  The posterior draw index.

## Value

The slots with a single `growth_rate`.
