# Refuse a primary event distribution for a summaries only meta model

The summaries only methods of
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md)
pass their extra arguments here. A `primary` among them would be
silently ignored otherwise, since summary rows take their tilt from the
`growth_rate` metadata.

## Usage

``` r
.assert_no_meta_primary(...)
```

## Arguments

- ...:

  The extra arguments of a summaries only method.

## Value

`NULL`, invisibly.
