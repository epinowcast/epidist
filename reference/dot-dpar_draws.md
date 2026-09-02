# Draws of each distributional parameter in a long `data.frame`

Draws of each distributional parameter in a long `data.frame`

## Usage

``` r
.dpar_draws(object, newdata = NULL, ...)
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

A `data.frame` with columns `.row`, `.chain`, `.iteration`, `.draw` and
one column per distributional parameter.
