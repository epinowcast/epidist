# The rows of the long format one `epireview` record contributes

The rows of the long format one `epireview` record contributes

## Usage

``` r
.epireview_row(record, type, value, se = NA_real_, p = NA_real_)
```

## Arguments

- record:

  The index of the record.

- type:

  The summary type of each row.

- value:

  The reported value of each row.

- se:

  The reported standard error of each row.

- p:

  The probability of each quantile row.

## Value

A `data.frame` with one row per value.
