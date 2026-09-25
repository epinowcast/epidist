# Posterior summaries of the delay mean and standard deviation of a fit

Posterior summaries of the delay mean and standard deviation of a fit

## Usage

``` r
.leave_one_out_summaries(fit, newdata, re_formula = NA, width)
```

## Arguments

- fit:

  A meta model fitted with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) to
  an `epidist_meta_model` object.

- newdata:

  A `data.frame` of data to predict the delay for, passed to
  [`delay_summary_draws()`](https://epidist.epinowcast.org/reference/delay_summary_draws.md).
  Always an already resolved `data.frame`, never `NULL`.

- re_formula:

  Passed to
  [`brms::prepare_predictions()`](https://paulbuerkner.com/brms/reference/prepare_predictions.html)
  through
  [`delay_summary_draws()`](https://epidist.epinowcast.org/reference/delay_summary_draws.md).
  The default `NA` switches off any study level term, so the summaries
  are of the population level delay.

- width:

  The width of the central posterior intervals. Defaults to `0.95`.

## Value

A `tibble` with one row per row of `newdata` and summary, with columns
`.row`, `summary`, `estimate` (the posterior median), `lower` and
`upper`.
