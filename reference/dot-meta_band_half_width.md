# The half width of the band of counts the forward pass keeps

The forward pass of
[`.meta_grid_box_ll()`](https://epidist.epinowcast.org/reference/dot-meta_grid_box_ll.md)
sums over the counts within six standard deviations and eight counts of
the most likely path, plus the stride of the grid that path was found
on. The mass beyond six standard deviations is below 1e-9 of the total
for every binomial, including the Poisson like tail of a step with a
small mean, which the eight counts cover. Matches
`meta_family_band_half_width()` in Stan.

## Usage

``` r
.meta_band_half_width(variance, stride)
```

## Arguments

- variance:

  The variance of the count.

- stride:

  The stride of the coarse grid from
  [`.meta_box_stride()`](https://epidist.epinowcast.org/reference/dot-meta_box_stride.md).

## Value

An integer number of counts.
