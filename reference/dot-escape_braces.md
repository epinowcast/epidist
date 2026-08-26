# Escape braces so that `cli` does not interpolate them

Messages from `checkmate` contain braces, which `cli` would otherwise
treat as glue interpolation.

## Usage

``` r
.escape_braces(x)
```

## Arguments

- x:

  A character vector.

## Value

`x` with each brace doubled.
