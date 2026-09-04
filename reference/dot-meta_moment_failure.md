# The summary vector returned when a normaliser underflows to zero

An infinite mean and standard deviation make the normal log likelihood
evaluate to `-Inf` for any finite reported value, so a draw that hits
this case is rejected rather than turning the log likelihood into `NaN`.

## Usage

``` r
.meta_moment_failure()
```

## Value

A named numeric vector with elements `mean`, `sd`, `kurtosis` and
`skewness`.
