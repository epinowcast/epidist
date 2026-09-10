# Label the strata of a `data.frame` of draws

Label the strata of a `data.frame` of draws

## Usage

``` r
.draw_strata(object, by = NULL)
```

## Arguments

- by:

  A character vector of columns of `object` that define the strata to
  colour by. If `NULL`, the default, the variables recorded on `object`
  are used. See the details.

## Value

A list with `data`, an ungrouped `tibble` of the draws with a `.stratum`
factor column, `by`, the columns the strata are defined by or `NULL`
when there is a single stratum, and `legend`, a legend title.
