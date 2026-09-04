# Interpolate a distribution function held at equally spaced points

Both the discrete grid and the accrual quadrature give the implied
distribution function at equally spaced delays, and a reported quantile
is read off by linear interpolation between them. Delays below the first
point are given zero and delays at or beyond the last are given one.

## Usage

``` r
.meta_interpolate(y, values, spacing, offset)
```

## Arguments

- y:

  A numeric vector of delays.

- values:

  The distribution function at the points.

- spacing:

  The distance between consecutive points.

- offset:

  The offset, in points, of the first point from a delay of zero. This
  is half a cell for the continuity corrected grid, which interpolates
  through the mid points of its cells, and zero for the accrual
  quadrature.

## Value

A numeric vector of probabilities.
