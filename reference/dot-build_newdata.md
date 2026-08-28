# Expand variables into a grid and add the variables a model needs

Used by the
[`epidist_newdata()`](https://epidist.epinowcast.org/reference/epidist_newdata.md)
methods. Any `epidist` class is dropped before expanding so that the
class methods documented in
[epidist_data](https://epidist.epinowcast.org/reference/epidist_data.md)
do not warn about an object that was never meant to stay in its class.

## Usage

``` r
.build_newdata(data, ..., .cols, .supplied = character())
```

## Arguments

- data:

  An `epidist` data object.

- ...:

  Variables to expand into a grid, passed to
  [`tidyr::expand()`](https://tidyr.tidyverse.org/reference/expand.html).

- .cols:

  A named list of the variables to add, each of which is crossed with
  the grid.

- .supplied:

  Names of the method's own arguments that the user gave, usually
  `intersect(names(match.call()), names(formals()))`. A name that was
  both expanded and supplied is an error. Names passed through `...` are
  not included, so setting a column with the
  [`tidyr::expand()`](https://tidyr.tidyverse.org/reference/expand.html)
  syntax, such as `pwindow = 1:2`, still works.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
of `newdata`.
