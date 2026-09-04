# The log of a difference of two exponentials

Matches Stan's `log_diff_exp()`, returning `-Inf` where the difference
is zero or negative rather than `NaN`.

## Usage

``` r
.meta_log_diff_exp(upper, lower)
```

## Arguments

- upper, lower:

  Logarithms, with `upper` expected to be the larger.

## Value

`log(exp(upper) - exp(lower))`.
