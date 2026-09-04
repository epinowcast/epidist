# The follow up available to a delay under an accrual design, up to a constant

Every use of the accrual weight renormalises afterwards, so the weights
are returned relative to their largest value to avoid overflow. A growth
rate of zero makes the follow up linear in the delay, which is taken
directly rather than through the log scale form.

## Usage

``` r
.meta_accrual_weight(d, window, growth_rate)
```

## Arguments

- d:

  A numeric vector of delays.

- window:

  The length of the collection window.

- growth_rate:

  The exponential growth rate of primary events.

## Value

A numeric vector of relative weights with a maximum of one.
