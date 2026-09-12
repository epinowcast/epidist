# Evaluate an expression, muffling the `brms` infinite data warning

Muffles only the warning
[`.infinite_data_warning()`](https://epidist.epinowcast.org/reference/dot-infinite_data_warning.md)
matches, through the `muffleWarning` restart, so every other warning
`expr` raises is left to propagate. The restart is only invoked when it
exists, so a condition signalled without one is passed on rather than
erroring.

## Usage

``` r
.muffle_infinite_data_warning(expr, muffle = TRUE)
```

## Arguments

- expr:

  An expression to evaluate.

- muffle:

  A logical. `expr` is evaluated unguarded when `FALSE`.

## Value

The value of `expr`.
