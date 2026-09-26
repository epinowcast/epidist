# The mean of a `brms` delay family

Replaces the `brms` internals `posterior_epred_<family>()`.

## Usage

``` r
.brms_family_posterior_epred(family, prep)
```

## Arguments

- family:

  The name of a `brms` family, for example `"lognormal"`.

- prep:

  A `brms` prepared predictions object.

## Value

The mean delay for every draw and observation.
