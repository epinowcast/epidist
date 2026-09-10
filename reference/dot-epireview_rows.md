# Map each `epireview` record to the rows of the long format

Map each `epireview` record to the rows of the long format

## Usage

``` r
.epireview_rows(data, meta)
```

## Arguments

- data:

  A `tibble` of `epireview` records.

- meta:

  The metadata of each record, as built by
  [`.epireview_metadata()`](https://epidist.epinowcast.org/reference/dot-epireview_metadata.md).

## Value

A `data.frame` with a `.record` column indexing the records and the
`type`, `value`, `se` and `p` of each row.
