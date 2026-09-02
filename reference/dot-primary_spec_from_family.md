# The primary event distribution of a family

Families built outside
[`epidist_family()`](https://epidist.epinowcast.org/reference/epidist_family.md),
and those made before this was configurable, carry no primary event
distribution and were uniform.

## Usage

``` r
.primary_spec_from_family(family)
```

## Arguments

- family:

  A `brms` family object.

## Value

The registry entry for the family's primary event distribution.
