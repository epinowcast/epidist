# Are the infinite values of `data` all observation times?

Are the infinite values of `data` all observation times?

## Usage

``` r
.inf_is_obs_time_only(data)
```

## Arguments

- data:

  A `data.frame`, or `NULL`.

## Value

`TRUE` when `data` holds at least one infinite value and every column
holding one is an
[`.obs_time_cols()`](https://epidist.epinowcast.org/reference/dot-obs_time_cols.md)
column, and `FALSE` otherwise.
