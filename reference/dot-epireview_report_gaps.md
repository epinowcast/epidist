# Message about the studies a metadata column left blank

A study assumed to have adjusted for right truncation is warned about,
as
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
warns.

## Usage

``` r
.epireview_report_gaps(meta, filled, kept)
```

## Arguments

- meta:

  The metadata of each record, as built by
  [`.epireview_metadata()`](https://epidist.epinowcast.org/reference/dot-epireview_metadata.md).

- filled:

  A list with one logical vector per column filled, as built by
  [`.epireview_metadata()`](https://epidist.epinowcast.org/reference/dot-epireview_metadata.md).

- kept:

  A logical vector marking the records kept.

## Value

`NULL`, invisibly, called for the messages it may raise.
