# The model-specific parts of an `epidist_family()` call

The model-specific parts of an
[`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md)
call

The model-specific parts of an
[`epidist_formula()`](https://epidist.epinowcast.org/reference/epidist_formula.md)
call

## Usage

``` r
epidist_family_model(data, family, ...)

epidist_formula_model(data, formula, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- family:

  Output of a call to
  [`brms::brmsfamily()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  with additional information as provided by
  [`.add_dpar_info()`](https://epidist.epinowcast.org/reference/dot-add_dpar_info.md)

- ...:

  Additional arguments passed to `fn` method.

- formula:

  An object of class
  [stats::formula](https://rdrr.io/r/stats/formula.html) or
  [brms::brmsformula](https://paulbuerkner.com/brms/reference/brmsformula.html)
  (or one that can be coerced to those classes). A symbolic description
  of the model to be fitted. A formula must be provided for the
  distributional parameter `mu`, and may optionally be provided for
  other distributional parameters.

## See also

Other family:
[`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md),
[`epidist_family_model.default()`](https://epidist.epinowcast.org/reference/epidist_family_model.default.md),
[`epidist_family_param()`](https://epidist.epinowcast.org/reference/epidist_family_param.md),
[`epidist_family_param.default()`](https://epidist.epinowcast.org/reference/epidist_family_param.default.md)

Other formula:
[`epidist_formula()`](https://epidist.epinowcast.org/reference/epidist_formula.md),
[`epidist_formula_model.default()`](https://epidist.epinowcast.org/reference/epidist_formula_model.default.md)
