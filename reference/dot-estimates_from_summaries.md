# Build an `epidist_estimates_data` object from one study's summaries

Build an `epidist_estimates_data` object from one study's summaries

## Usage

``` r
.estimates_from_summaries(study, value, type, p, se, n, ...)
```

## Arguments

- study:

  A string naming the study.

- value:

  A numeric vector of reported summaries.

- type:

  A character vector of summary types, one per `value`.

- p:

  A numeric vector of quantile probabilities, one per `value`.

- se:

  A numeric vector of reported standard errors, or `NULL`.

- n:

  The number of delays the study summarised, or `NULL`.

- ...:

  Study metadata.

## Value

An `epidist_estimates_data` object.
