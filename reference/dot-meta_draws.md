# Implied summaries shared by meta model rows with the same study design

Holds one entry per study design, each a list of the parameter draws it
was built from and the summaries they imply. It lives in the package
namespace, so it is never written into a fitted model object. See
[`.meta_row_draw_moments()`](https://epidist.epinowcast.org/reference/dot-meta_row_draw_moments.md).

## Usage

``` r
.meta_draws
```

## Format

An environment.
