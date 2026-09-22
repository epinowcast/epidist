# Take the observed delays of a fitted model

Reads the response and the case weights of the model data, and the
strata to colour by, which default to the variables in the
distributional parameter formulas as they do in
[`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md).

## Usage

``` r
.fitted_delays(x, by = NULL)
```

## Arguments

- x:

  An `epidist_linelist_data` or `epidist_aggregate_data` object, a named
  list of them to compare, or a model fitted with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

- by:

  A character vector of columns of the model data that define the strata
  to colour by. If `NULL`, the default, the variables in the
  distributional parameter formulas are used, as
  [`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)
  does.

## Value

A list with `data`, a `tibble` of the observed `delay` of each case, its
weight `n` and the columns in `by`, and `by`, the columns the strata are
defined by or `NULL` when there are none.
