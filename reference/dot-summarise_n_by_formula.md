# Summarise data by grouping variables and count occurrences

Summarise data by grouping variables and count occurrences

## Usage

``` r
.summarise_n_by_formula(data, by = character(), formula = NULL)
```

## Arguments

- data:

  A `data.frame` to summarise which must contain a `n` column which is a
  count of occurrences.

- by:

  Character vector of column names to group by.

- formula:

  Optional `brms` formula object to extract additional grouping terms
  from.

## Value

A `data.frame` summarised by the grouping variables with counts.
