# Name the rows of `newdata` by the columns that differ between them

Name the rows of `newdata` by the columns that differ between them

## Usage

``` r
.stratum_names(newdata)
```

## Arguments

- newdata:

  A `data.frame` of data to predict for, with one row per delay
  distribution wanted. If `NULL`, the default,
  [`epidist_newdata()`](https://epidist.epinowcast.org/reference/epidist_newdata.md)
  builds one row per unique combination of the predictors, with no
  censoring and no truncation. See the details.

## Value

A character vector with one element per row of `newdata`.
