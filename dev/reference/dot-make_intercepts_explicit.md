# Include implicit intercepts in `brms` formula as explicit

This function detects the distributional parameters in a `brms` formula
object, and alters to formula to include explicit intercept parameters
for them i.e. `~ 1`.

## Usage

``` r
.make_intercepts_explicit(formula)
```

## Arguments

- formula:

  A `brms` formula object.
