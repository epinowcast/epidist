# Round to the nearest multiple

This function rounds an input `x` down to the nearest multiple of some
number `f`. For example, if `f = 0.2` and `x = 1.5` then the output
would be 1.4. If `f = 1` then `floor_mult` behaves as `floor`. If
`f = 0` then `x` is returned.

## Usage

``` r
.floor_mult(x, f = 1)
```

## Arguments

- x:

  A number to be rounded down.

- f:

  A positive number specifying the multiple to be rounded down to
