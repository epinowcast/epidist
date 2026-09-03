# Check if data has the `epidist_data` class

All `epidist` data objects carry this class in addition to their
specific class. See
[epidist_data](https://epidist.epinowcast.org/reference/epidist_data.md)
for the methods it provides.

## Usage

``` r
is_epidist_data(data, ...)
```

## Arguments

- data:

  An object.

- ...:

  Additional arguments

## Value

A logical, `TRUE` if `data` inherits from `epidist_data` and `FALSE`
otherwise.

## See also

Other epidist_data:
[`epidist_data`](https://epidist.epinowcast.org/reference/epidist_data.md)

## Examples

``` r
sierra_leone_ebola_data |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ) |>
  is_epidist_data()
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).
#> [1] TRUE
```
