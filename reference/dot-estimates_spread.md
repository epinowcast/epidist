# The spread each study reported, as a proxy for its delay standard deviation

The quadrature a summary row uses has to resolve the delay distribution,
whose scale is only known once the model is fitted. The spread the study
itself reported stands in for it. That is its reported standard
deviation where it gave one, the range of its reported quantiles divided
by the same range of a standard normal where it gave two or more, and
otherwise a quarter of the smallest location it reported, which is the
coefficient of variation below which a delay is narrow. The proxy only
has to be within a factor of two or so, because the resolution is chosen
well inside where Simpson's rule converges.

## Usage

``` r
.estimates_spread(data)
```

## Arguments

- data:

  An `epidist_estimates_data` object.

## Value

A numeric vector of spreads, one per row.
