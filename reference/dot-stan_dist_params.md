# The Stan expression for the parameter array of a delay distribution

The functions chunks pass the delay distribution parameters to
`primarycensored` as the array `{dpars_B}`. For a parametric family that
is the distributional parameters in the `primarycensored` order,
`pcd_param` where the family records one and `param` otherwise, in
braces. The non-parametric family builds the array of boundaries and
hazards with `epidist_np_params()`, which is already an array, see
[`epidist_family_param()`](https://epidist.epinowcast.org/reference/epidist_family_param.md).

## Usage

``` r
.stan_dist_params(family)
```

## Arguments

- family:

  The `epidist` family object.

## Value

A character string holding a Stan expression.
