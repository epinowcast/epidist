# Define a model specific formula

This function is used within
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) to
create the formula object passed to `brms`. It is unlikely that as a
user you will need this function, but we export it nonetheless to be
transparent about what exactly is happening inside of a call to
[`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

## Usage

``` r
epidist_formula(data, family, formula, ...)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

- family:

  A description of the response distribution and link function to be
  used in the model created using
  [`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md).

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
[`epidist_family_model()`](https://epidist.epinowcast.org/reference/epidist_family_model.md),
[`epidist_formula_model.default()`](https://epidist.epinowcast.org/reference/epidist_formula_model.default.md)
