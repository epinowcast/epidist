# Posterior draws of the delay distribution parameters

Returns posterior draws of the parameters of the delay distribution in
the long format used by `tidybayes`. The delay parameters are the
distributional parameters of the `brms` family, evaluated on the
response scale for each row of `newdata`. For a lognormal model they are
`mu` and `sigma`. They are the parameters of the delay distribution
itself, so they do not describe the censoring or truncation of the
observation process, and they are not the natural scale mean and
standard deviation of the delay. Use
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md)
to add those.

`add_delay_parameter_draws()` is the same function with `newdata` first,
for use at the start of a pipeline as with
[`tidybayes::add_epred_draws()`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html).

## Usage

``` r
delay_parameter_draws(object, newdata = NULL, ...)

add_delay_parameter_draws(newdata, object, ...)
```

## Arguments

- object:

  A model fit with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

- newdata:

  A `data.frame` of data to predict for. If `NULL`, the default, the
  data the model was fitted to is used. The `brms` models `epidist` fits
  need the model variables as well as the predictors, so build `newdata`
  with
  [`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)
  rather than from the predictors alone.

- ...:

  Additional arguments passed to
  [`brms::prepare_predictions()`](https://paulbuerkner.com/brms/reference/prepare_predictions.html),
  such as `ndraws` or `re_formula`.

## Value

A `tibble` of posterior draws of the delay distribution parameters,
grouped by the columns of `newdata` and by `.row`.

## Details

The returned columns follow the `tidybayes` conventions. The columns of
`newdata` come first, followed by `.row`, `.chain`, `.iteration` and
`.draw`, followed by one column per distributional parameter. The result
is grouped by the columns of `newdata` and by `.row`. `.chain` and
`.iteration` are `NA` when the draws have been subset, because the chain
a subset draw came from is not recoverable.

Every row of `newdata` gets its own draws, so passing the data the model
was fitted to produces many identical draws when the model has few
unique combinations of predictors.
[`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)
returns one row per unique combination and is usually the better input.

## See also

[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md)
to add natural scale summaries of the delay,
[`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)
to build `newdata`.

Other postprocess:
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md),
[`epidist_strata()`](https://epidist.epinowcast.org/reference/epidist_strata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# `fit` is a model fitted with `epidist()`
fit |>
  epidist_strata() |>
  add_delay_parameter_draws(fit) |>
  add_summaries(probs = c(0.05, 0.95))
} # }
```
