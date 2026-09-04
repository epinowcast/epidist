# A numerical Jacobian of the map from parameters to summaries

Uses a central difference with a step relative to each parameter, held
away from zero so that a parameter reported as zero still moves. The
step of a parameter constrained to be positive is held below half its
value, so the lower evaluation stays inside the support.

## Usage

``` r
.estimates_delta_jacobian(fn, x, positive)
```

## Arguments

- fn:

  A function of the parameter vector returning the summaries.

- x:

  The reported parameters.

- positive:

  A logical vector marking the parameters constrained to be positive.

## Value

A matrix with one row per summary and one column per parameter.

## Details

A numerical Jacobian is used because the derivative of a quantile with
respect to the shape of a gamma or a weibull has no closed form.
