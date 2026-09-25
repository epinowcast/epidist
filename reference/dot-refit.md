# Refit a model to new data, reusing the compiled model

Refit a model to new data, reusing the compiled model

## Usage

``` r
.refit(fit, newdata, ...)
```

## Arguments

- fit:

  A meta model fitted with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) to
  an `epidist_meta_model` object.

- newdata:

  The data to refit to.

- ...:

  Additional arguments passed to
  [`brms::update.brmsfit()`](https://paulbuerkner.com/brms/reference/update.brmsfit.html)
  and so to
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html),
  such as `cores`, `chains`, `iter`, `refresh` and `silent`.

## Value

A fit with the classes of `fit`.
