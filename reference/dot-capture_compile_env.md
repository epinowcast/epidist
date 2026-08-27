# Capture the environment variables `rstan` leaks when compiling a model

The `rstan` backend compiles through
[`inline::cxxfunction()`](https://rdrr.io/pkg/inline/man/cxxfunction.html),
which sets `PKG_CPPFLAGS` and `PKG_LIBS` and never restores them. The
leaked flags make the next
[`pkgbuild::has_build_tools()`](https://pkgbuild.r-lib.org/reference/has_build_tools.html)
check fail, which prints a spurious compiler error before the model
compiles and fits successfully. Restore them with
[`.restore_compile_env()`](https://epidist.epinowcast.org/reference/dot-restore_compile_env.md)
after a fit.

## Usage

``` r
.capture_compile_env()
```

## Value

A named character vector of the current values, `NA` where unset.
