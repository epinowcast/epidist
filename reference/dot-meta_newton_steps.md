# The number of Newton steps taken from the chord inverse

Matches `meta_family_node_quantile()` in Stan. Each step squares the
error of the chord, which is of the order of the node spacing squared,
so two steps leave a residual well below the sampling standard error of
any reported quantile.

## Usage

``` r
.meta_newton_steps()
```

## Value

An integer.
