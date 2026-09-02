# Unique combinations of the predictors in a model

Returns one row of the model data for each unique combination of the
variables used to predict the delay distribution parameters. Passing
this to
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)
gives one set of draws per combination rather than one per observation,
which is the same result with far fewer draws.

## Usage

``` r
epidist_strata(object, vars = NULL)
```

## Arguments

- object:

  A model fit with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

- vars:

  A character vector of variables to take unique combinations of. If
  `NULL`, the default, the variables in the distributional parameter
  formulas are used.

## Value

A `tibble` with one row per unique combination of `vars`, with the
combination columns first.

## Details

The variables are taken from the right hand side of each distributional
parameter formula. The remaining columns are kept from the first row of
the model data in which each combination occurs. This keeps the model
variables that `brms` requires in `newdata`, such as the relative
observation time and the censoring windows for the latent and marginal
models. Those variables do not enter the distributional parameters, so
the values kept do not change the draws.

A model with only an intercept has no predictors and so returns a single
row. A continuous predictor has as many combinations as it has distinct
values, so consider passing `vars` and a grid of your own instead.

## See also

Other postprocess:
[`add_summaries()`](https://epidist.epinowcast.org/reference/add_summaries.md),
[`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# `fit` is a model fitted with `epidist()`
epidist_strata(fit)
} # }
```
