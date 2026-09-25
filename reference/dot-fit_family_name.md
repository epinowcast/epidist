# The name of the `brms` family a fit was made with

The families `epidist` builds are custom families named after the model
and the delay distribution, such as `meta_lognormal`.

## Usage

``` r
.fit_family_name(fit)
```

## Arguments

- fit:

  A meta model fitted with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md) to
  an `epidist_meta_model` object.

## Value

A string.
