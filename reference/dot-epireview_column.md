# A column of an `epireview` table, or `NA` where it is absent

A column of an `epireview` table, or `NA` where it is absent

## Usage

``` r
.epireview_column(data, names)
```

## Arguments

- data:

  A `tibble` of `epireview` records.

- names:

  The names the column may have, tried in order.

## Value

The first of the columns present, or a vector of `NA` with one entry per
record where none is.
