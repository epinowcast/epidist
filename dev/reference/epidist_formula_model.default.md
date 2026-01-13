# Default method for defining a model specific formula

Default method for defining a model specific formula

## Usage

``` r
# Default S3 method
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

## See also

Other formula:
[`epidist_family_model()`](https://epidist.epinowcast.org/dev/reference/epidist_family_model.md),
[`epidist_formula()`](https://epidist.epinowcast.org/dev/reference/epidist_formula.md)
