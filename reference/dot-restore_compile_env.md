# Restore environment variables captured by [`.capture_compile_env()`](https://epidist.epinowcast.org/reference/dot-capture_compile_env.md)

Restore environment variables captured by
[`.capture_compile_env()`](https://epidist.epinowcast.org/reference/dot-capture_compile_env.md)

## Usage

``` r
.restore_compile_env(vars)
```

## Arguments

- vars:

  A named character vector as returned by
  [`.capture_compile_env()`](https://epidist.epinowcast.org/reference/dot-capture_compile_env.md),
  with `NA` for variables that were unset.

## Value

Nothing, called for side effects only
