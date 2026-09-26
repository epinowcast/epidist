# The post-processing functions of a delay family

Gives the functions with the signatures `brms` expects of a custom
family, for a delay family `brms` provides. Other families get functions
that error when called, so a model can still be fitted with them.

## Usage

``` r
.brms_family_fn(prefix, family)
```

## Arguments

- prefix:

  One of `"log_lik"`, `"posterior_predict"` or `"posterior_epred"`.

- family:

  The name of a `brms` family, for example `"lognormal"`.

## Value

A function.
