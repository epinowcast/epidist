# The counts of a coarse grid within a box

The box boundaries and every multiple of the stride between them, so
that a most likely count sitting on a boundary is on the grid.

## Usage

``` r
.meta_box_grid(lower, upper, stride)
```

## Arguments

- lower, upper:

  The box.

- stride:

  The stride of the grid from
  [`.meta_box_stride()`](https://epidist.epinowcast.org/reference/dot-meta_box_stride.md).

## Value

An increasing integer vector of counts.
