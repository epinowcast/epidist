# A numerically stable log of a sum of exponentials

A numerically stable log of a sum of exponentials

## Usage

``` r
.meta_log_sum_exp(x)
```

## Arguments

- x:

  A numeric vector of log values.

## Value

`log(sum(exp(x)))`, or `-Inf` when every element is `-Inf`.
