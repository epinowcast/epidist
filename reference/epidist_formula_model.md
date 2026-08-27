# The model-specific parts of an `epidist_formula()` call

The model-specific parts of an
[`epidist_formula()`](https://epidist.epinowcast.org/reference/epidist_formula.md)
call

## Usage

``` r
epidist_formula_model(data, formula, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- formula:

  An object of class
  [stats::formula](https://rdrr.io/r/stats/formula.html) or
  [brms::brmsformula](https://paulbuerkner.com/brms/reference/brmsformula.html)
  (or one that can be coerced to those classes). A symbolic description
  of the model to be fitted. A formula must be provided for the
  distributional parameter `mu`, and may optionally be provided for
  other distributional parameters.

- ...:

  Additional arguments passed to `fn` method.

## Value

A `brmsformula` object.

## See also

Other formula:
[`epidist_formula()`](https://epidist.epinowcast.org/reference/epidist_formula.md),
[`epidist_formula_model.default()`](https://epidist.epinowcast.org/reference/epidist_formula_model.default.md)
