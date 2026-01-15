# Assert validity of `epidist_aggregate_data` objects

Assert validity of `epidist_aggregate_data` objects

## Usage

``` r
# S3 method for class 'epidist_aggregate_data'
assert_epidist(data, ...)
```

## Arguments

- data:

  An object to check

- ...:

  Additional arguments

## See also

Other aggregate_data:
[`as_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.md),
[`as_epidist_aggregate_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.data.frame.md),
[`as_epidist_aggregate_data.default()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.default.md),
[`as_epidist_aggregate_data.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/as_epidist_aggregate_data.epidist_linelist_data.md),
[`is_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/is_epidist_aggregate_data.md),
[`new_epidist_aggregate_data()`](https://epidist.epinowcast.org/reference/new_epidist_aggregate_data.md)

## Examples

``` r
sierra_leone_ebola_data |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  ) |>
  as_epidist_aggregate_data() |>
  assert_epidist()
#> ℹ No primary event upper bound provided, using the primary event lower bound + 1 day as the assumed upper bound.
#> ℹ No secondary event upper bound provided, using the secondary event lower bound + 1 day as the assumed upper bound.
#> ℹ No observation time column provided, using 2015-09-14 as the observation date (the maximum of the secondary event upper bound).
```
