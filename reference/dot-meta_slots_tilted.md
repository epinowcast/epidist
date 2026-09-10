# Whether the primary event of a meta model row is tilted

Mirrors the primary event id of `meta_family_lpmf()` in Stan, which
takes the exponential growth path for every row that estimates its rate,
whatever value the parameter holds, and for a known rate other than
zero.

## Usage

``` r
.meta_slots_tilted(slots)
```

## Arguments

- slots:

  The output of
  [`.meta_draw_slots()`](https://epidist.epinowcast.org/reference/dot-meta_draw_slots.md).

## Value

`TRUE` where the primary event is not uniform.
