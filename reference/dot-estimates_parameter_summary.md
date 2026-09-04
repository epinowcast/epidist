# The summaries a reported distribution implies

The summaries a reported distribution implies

## Usage

``` r
.estimates_parameter_summary(
  family,
  parameters,
  moments,
  probs,
  lower = 0,
  cutoff = Inf
)
```

## Arguments

- family:

  The distribution the study fitted.

- parameters:

  A named numeric vector of the reported parameters.

- moments:

  Which moments to report, any of `"mean"` and `"sd"`, in that order.

- probs:

  A numeric vector of probabilities to report quantiles at.

- lower:

  The smallest delay the study counted.

- cutoff:

  The largest delay the study could have seen, or `Inf` where it
  adjusted for right truncation.

## Value

A numeric vector of summaries, the moments first.
