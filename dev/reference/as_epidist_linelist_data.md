# Create an epidist_linelist_data object

Creates an epidist_linelist_data object from various input formats. This
is useful when working with individual-level data where each row
represents a single observation with primary and secondary event times.
See the specific methods for details on supported input formats and
usage examples.

## Usage

``` r
as_epidist_linelist_data(data, ...)
```

## Arguments

- data:

  The data to convert

- ...:

  Additional arguments passed to methods

## See also

Other linelist_data:
[`as_epidist_linelist_data.data.frame()`](https://epidist.epinowcast.org/dev/reference/as_epidist_linelist_data.data.frame.md),
[`as_epidist_linelist_data.default()`](https://epidist.epinowcast.org/dev/reference/as_epidist_linelist_data.default.md),
[`as_epidist_linelist_data.epidist_aggregate_data()`](https://epidist.epinowcast.org/dev/reference/as_epidist_linelist_data.epidist_aggregate_data.md),
[`assert_epidist.epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/assert_epidist.epidist_linelist_data.md),
[`is_epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/is_epidist_linelist_data.md),
[`new_epidist_linelist_data()`](https://epidist.epinowcast.org/dev/reference/new_epidist_linelist_data.md)
