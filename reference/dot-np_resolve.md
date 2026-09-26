# Resolve the non-parametric family against the model data

Checks that the model supports the family, sets the default boundaries
from the data where none were given, and checks that the boundaries
reach the longest observed delay.

## Usage

``` r
.np_resolve(family, data)
```

## Arguments

- family:

  A family built by
  [`nonparametric()`](https://epidist.epinowcast.org/reference/nonparametric.md).

- data:

  An object with class corresponding to an implemented model.

## Value

The family with its boundaries set.
