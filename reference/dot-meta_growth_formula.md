# The default `pgrowth` formula of a meta model

See
[`epidist_formula_model.epidist_meta_model()`](https://epidist.epinowcast.org/reference/epidist_formula_model.epidist_meta_model.md).

## Usage

``` r
.meta_growth_formula(data, formula)
```

## Arguments

- data:

  An `epidist_meta_model` object.

- formula:

  A `brmsformula` object carrying its family.

## Value

The formula, with `pgrowth ~ 0 + study`, or `pgrowth ~ 1` for a single
study, added where a summary row estimates its growth rate and no
`pgrowth` formula was given.
