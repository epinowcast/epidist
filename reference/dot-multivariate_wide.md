# Widen a data frame of draws into one column per element

Widen a data frame of draws into one column per element

## Usage

``` r
.multivariate_wide(draws, params, index, draw, index_values)
```

## Arguments

- draws:

  A data frame of draws.

- params:

  The parameter columns.

- index:

  The index column, or `NULL`.

- draw:

  The draw column, or `NULL`.

- index_values:

  The trajectory points, in order.

## Value

A numeric matrix with one row per draw and one column per element,
ordered index major and parameter minor.
