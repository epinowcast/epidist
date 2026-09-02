# The primary event distribution a fit was made with

The post-processing functions are built from a family, which the caller
may supply directly rather than taking it from the fit. The fit itself
is authoritative, so prefer what it carries and fall back on the family.

## Usage

``` r
.primary_spec_from_prep(prep, spec)
```

## Arguments

- prep:

  A `brms` prep object.

- spec:

  The registry entry of the family the caller supplied.

## Value

A registry entry.
