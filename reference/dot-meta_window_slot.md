# The censoring window slot of a summary row

A fully adjusted study (`cens_adjusted` code 1) may leave its windows
`NA` in
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md),
because none of its estimands read them. The slot still has to hold a
number for Stan, so a missing window is filled with 1, which no code 1
path uses.

## Usage

``` r
.meta_window_slot(window)
```

## Arguments

- window:

  A censoring window width, possibly `NA`.

## Value

A number.
