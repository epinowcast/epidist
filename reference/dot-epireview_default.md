# The default `as_epidist_estimates_data()` assumes for a metadata column

Reads from
[.estimates_default_values](https://epidist.epinowcast.org/reference/dot-estimates_default_values.md)
so the fallback matches
[`.fill_estimates_defaults()`](https://epidist.epinowcast.org/reference/dot-fill_estimates_defaults.md)
without restating each value here.

## Usage

``` r
.epireview_default(col, meta)
```

## Arguments

- col:

  The column.

- meta:

  The metadata of each record, as built by
  [`.epireview_metadata()`](https://epidist.epinowcast.org/reference/dot-epireview_metadata.md).

## Value

A vector with one default per record.
