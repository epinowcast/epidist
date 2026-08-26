# Validate a model formula

Replaces `brms:::validate_formula()` for the families `epidist`
supports. Guarantees that the result is a `brmsformula` object carrying
the validated family, with any `.` on the right hand side expanded
against `data` and with `mecor` set to its default of `TRUE`. The `brms`
internal additionally handles ordinal, categorical, mixture and Cox
families, and the deprecated `autocor`, `sparse` and `cov_ranef`
arguments. None of those are supported here.

## Usage

``` r
.validate_formula(formula, family = NULL, data = NULL)
```

## Arguments

- formula:

  A formula or `brmsformula` object.

- family:

  A description of the response distribution and link function.

- data:

  A `data.frame` used to expand `.` in the formula.
