# The stride of the coarse grid of counts the forward pass centres on

The most likely path of the constrained chain of counts is found on a
grid of every `stride`th count, about half a standard deviation of a
binomial apart, so that a search over it costs about four times the
sample size per edge. Matches `meta_family_box_stride()` in Stan.

## Usage

``` r
.meta_box_stride(study_n)
```

## Arguments

- study_n:

  The number of delays the quantiles were computed from.

## Value

An integer stride of at least one.
