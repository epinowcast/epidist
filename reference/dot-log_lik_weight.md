# Apply observation weights to a log likelihood

Replaces `brms:::log_lik_weight()`. Multiplies `x` by the weight of
observation `i`, if the model has weights, and returns `x` unchanged
otherwise.

## Usage

``` r
.log_lik_weight(x, i, prep)
```

## Arguments

- x:

  A numeric vector of log likelihood values.

- i:

  The index of the observation.

- prep:

  A `brms` prepared predictions object.
