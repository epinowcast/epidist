# The discrete colour scale the package plots use

Uses
[`.epidist_palette()`](https://epidist.epinowcast.org/reference/dot-epidist_palette.md)
when it holds enough colours and the viridis scale otherwise. The scale
covers both the `colour` and the `fill` aesthetic, so that a variable
mapped to each gets one legend.

## Usage

``` r
.epidist_colour_scale(n)
```

## Arguments

- n:

  The number of levels to colour.

## Value

A `ggplot2` scale.
