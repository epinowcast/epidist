# Check the hazard formula of the non-parametric family

Check the hazard formula of the non-parametric family

## Usage

``` r
.np_check_formula(formula)
```

## Arguments

- formula:

  A one sided formula for the logit hazards over the bins, see Details.
  The default, `NULL`, is a spline over the delay.

## Value

The formula, with any `(1 | bin)` term written as `s(bin, bs = "re")`.
