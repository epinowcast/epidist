# Add summaries from an analytic solution

Add summaries from an analytic solution

## Usage

``` r
.analytic_summaries(data, analytic, probs = NULL)
```

## Arguments

- data:

  A `data.frame` of draws of the distributional parameters, as returned
  by
  [`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md).

- analytic:

  A list of analytic solutions, as returned by
  [`.analytic_delay_summaries()`](https://epidist.epinowcast.org/reference/dot-analytic_delay_summaries.md).

- probs:

  A numeric vector of probabilities to add quantiles of the delay
  distribution for. If `NULL`, the default, no quantiles are added.

## Value

The input with summary columns added.
