# The default `newdata` for comparing leave one out refits

A single row giving the population level delay with no censoring and no
truncation.
[`epidist_newdata()`](https://epidist.epinowcast.org/reference/epidist_newdata.md)
only reads its `data` argument to expand variables, so a copy of the
model data carrying the class is enough.

## Usage

``` r
.leave_one_out_newdata(fit)
```

## Arguments

- fit:

  A meta model fitted with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) to
  an `epidist_meta_model` object.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
of one row.
