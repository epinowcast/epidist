# Add delay_min column to data

Resolves the `delay_min` argument into a column on the data frame. If
NULL, uses an existing `delay_min` column or defaults to 0. If numeric,
uses that scalar. If character, looks up the named column.

## Usage

``` r
.add_delay_min(data, delay_min = NULL)
```

## Arguments

- data:

  A data frame

- delay_min:

  NULL, a numeric scalar, or a column name string

## Value

The data frame with a `delay_min` column
