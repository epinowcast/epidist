# Resolve the study metadata of each `epireview` record

Starts from the metadata shared by every record, replaces it with the
values `metadata` gives for each study, and fills the studies a column
leaves blank with the default
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
would assume. A column no study has a value for is left out, so that
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
assumes it and messages once. The studies filled are reported by
[`.epireview_report_gaps()`](https://epidist.epinowcast.org/reference/dot-epireview_report_gaps.md)
once the records kept are known, so that a dropped record is not named.

## Usage

``` r
.epireview_metadata(studies, n, metadata, shared)
```

## Arguments

- studies:

  A character vector naming the study of each record.

- n:

  A numeric vector of the sample size of each record.

- metadata:

  A `data.frame` of study metadata, or `NULL`.

- shared:

  A named list of metadata applied to every record.

## Value

A list holding `meta`, a `tibble` with one row per record and the
`study`, `n` and metadata columns that were given, and `filled`, a list
with one logical vector per column filled marking the records filled.
