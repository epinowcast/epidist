# The standard deviation an `epireview` record reports alongside its mean

The standard deviation an `epireview` record reports alongside its mean

## Usage

``` r
.epireview_sd(data, i, spread, spread_type)
```

## Arguments

- data:

  A `tibble` of `epireview` records.

- i:

  The record.

- spread:

  The single uncertainty value of each record.

- spread_type:

  The single uncertainty type of each record.

## Value

The standard deviation, or `NA` where none is reported. A standard
deviation of zero is treated as not reported by the caller, because
[`as_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.md)
rejects it.
