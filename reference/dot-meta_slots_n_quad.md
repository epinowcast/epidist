# The number of quadrature intervals a row's slots ask for

Rows built by
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md)
carry it in their `n_quad` slot. A slots list assembled by hand without
one uses the floor.

## Usage

``` r
.meta_slots_n_quad(slots)
```

## Arguments

- slots:

  The output of
  [`.meta_row_slots()`](https://epidist.epinowcast.org/reference/dot-meta_row_slots.md).

## Value

An integer number of intervals.
