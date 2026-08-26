# Expand a dot in a model formula

Replaces the part of `brms:::expand_dot_formula()` that `epidist` needs.
A formula containing `.` on the right hand side is expanded against
`data`, keeping the attributes of the original formula.

## Usage

``` r
.expand_dot_formula(formula, data = NULL)
```

## Arguments

- formula:

  A formula object.

- data:

  A `data.frame` used to expand `.`.
