# The delay distribution family of a fitted model

The families `epidist` builds are `brms` custom families named after the
model and the delay distribution, such as `latent_lognormal`. This
returns the delay distribution part, which is what the summaries and the
simulation need.

## Usage

``` r
.delay_family(family)
```

## Arguments

- family:

  A `brms` family.

## Value

A list with the delay distribution `name`, its distributional parameters
`dpars`, and for the non-parametric family its boundaries and hazard
model as `np`.
