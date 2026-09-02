# Add the distributional parameters a primary event distribution needs

Also records the distribution on the family so that the post-processing
functions built from it use the same one.

## Usage

``` r
.add_primary_dpars(family, data)
```

## Arguments

- family:

  A `brms` family object.

- data:

  An `epidist` data object.

## Value

The family with any primary event parameters added.
