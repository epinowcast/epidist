# Add an independent uniform primary window to a set of summaries

The uniform single interval approximation leaves the primary interval
uncorrected, so the study effectively summarised the delay plus an
independent draw from a uniform distribution over the primary window.
This convolution is exact when the study also adjusted for right
truncation and the primary events were uniform within their window.

## Usage

``` r
.meta_add_uniform(moments, pwindow)
```

## Arguments

- moments:

  A summary vector from
  [`.meta_moment_vector()`](https://epidist.epinowcast.org/reference/dot-meta_moment_vector.md).

- pwindow:

  The primary censoring window width.

## Value

A named numeric vector with elements `mean`, `sd`, `kurtosis` and
`skewness`.
