# Default `newdata` for a fitted model

Expands the variables in the distributional parameter formulas into a
grid with
[`epidist_newdata()`](https://epidist.epinowcast.org/reference/epidist_newdata.md),
so there is one row per unique combination of the predictors with no
censoring and no truncation. `brms` keeps the data a model was fitted to
as a plain `data.frame`, so the `epidist` class is restored from the
family name first.

## Usage

``` r
.fit_newdata(object)
```

## Arguments

- object:

  A model fit with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
of `newdata`.
