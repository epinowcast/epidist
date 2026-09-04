# Summarise draws of a study's fitted parameters

Summarise draws of a study's fitted parameters

## Usage

``` r
.estimates_mvn_summarise(data, family, moments, probs, ...)
```

## Arguments

- data:

  An `epidist_multivariate` object holding draws of the natural
  parameters of a fitted distribution.

- family:

  The distribution the study fitted.

- moments:

  Which moments to report.

- probs:

  A numeric vector of probabilities to report quantiles at.

- ...:

  Study metadata, used for the range the summaries are taken over.

## Value

A list with the `type`, `p`, `value` and `vcov` of the summaries.
