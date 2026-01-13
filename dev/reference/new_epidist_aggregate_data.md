# Class constructor for `epidist_aggregate_data` objects

Class constructor for `epidist_aggregate_data` objects

## Usage

``` r
new_epidist_aggregate_data(data)
```

## Arguments

- data:

  A data.frame to convert

## Value

An object of class `epidist_aggregate_data`

## See also

Other aggregate_data:
[`as_epidist_aggregate_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_aggregate_data.md),
[`as_epidist_aggregate_data.data.frame()`](https://epidist.epinowcast.org/dev/reference/as_epidist_aggregate_data.data.frame.md),
[`as_epidist_aggregate_data.default()`](https://epidist.epinowcast.org/dev/reference/as_epidist_aggregate_data.default.md),
[`as_epidist_aggregate_data.epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_aggregate_data.epidist_linelist_data.md),
[`assert_epidist.epidist_aggregate_data()`](https://epidist.epinowcast.org/dev/reference/assert_epidist.epidist_aggregate_data.md),
[`is_epidist_aggregate_data()`](https://epidist.epinowcast.org/dev/reference/is_epidist_aggregate_data.md)

## Examples

``` r
df <- new_epidist_aggregate_data(data.frame())
class(df)
#> [1] "epidist_aggregate_data" "data.frame"            
```
