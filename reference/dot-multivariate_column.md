# Resolve an optional column name of a data frame of draws

Resolve an optional column name of a data frame of draws

## Usage

``` r
.multivariate_column(supplied, draws, default)
```

## Arguments

- supplied:

  The column name the user gave, or `NULL`.

- draws:

  A data frame of draws.

- default:

  The column name to fall back on where the data frame has one.

## Value

A column name, or `NULL`.
