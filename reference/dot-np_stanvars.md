# Stan functions of the non-parametric family

`epidist_np_params()`, and the functions `epidist_np_boundaries()` and
`epidist_np_basis()`, which return the boundaries and the basis of the
family as constants.

## Usage

``` r
.np_stanvars(family)
```

## Arguments

- family:

  The `epidist` family object.

## Value

A `brms` `stanvars` object, or `NULL` for any other family.
