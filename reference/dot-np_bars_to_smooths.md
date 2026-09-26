# Write random intercepts per bin as random effect smooths

Write random intercepts per bin as random effect smooths

## Usage

``` r
.np_bars_to_smooths(formula)
```

## Arguments

- formula:

  A one sided formula for the logit hazards over the bins, see Details.
  The default, `NULL`, is a spline over the delay.

## Value

The formula with each `(1 | g)` term replaced by `s(g, bs = "re")`.
