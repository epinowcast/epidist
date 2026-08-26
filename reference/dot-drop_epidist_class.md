# Drop `epidist` classes from an object

Removes `class` from `data`, and also removes the shared `epidist_data`
class once no specific `epidist` class is left.

## Usage

``` r
.drop_epidist_class(data, class = NULL)
```

## Arguments

- data:

  An object to drop classes from.

- class:

  A character vector of `epidist` classes to drop. Defaults to `NULL`,
  in which case all `epidist` classes are dropped.

## Value

`data` with the requested classes removed.
