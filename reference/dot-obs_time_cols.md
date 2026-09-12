# Columns that `epidist` may set to an infinite observation time

The relative observation time is infinite when the delay is not right
truncated, which is the default of the
[`epidist_newdata()`](https://epidist.epinowcast.org/reference/epidist_newdata.md)
methods. The marginal model also sets it for observation times far
beyond the longest delay, keeping the original in
`orig_relative_obs_time`.

## Usage

``` r
.obs_time_cols()
```

## Value

A character vector of column names.
