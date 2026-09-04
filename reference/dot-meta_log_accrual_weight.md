# The log follow up available to a delay under an accrual design

A study that collected primary events over a window of length `window`
and stopped at its calendar end could only observe a delay of `d` for
the primary events that occurred at least `d` before the stop. With
primary events arriving at a rate proportional to \\\exp(r t)\\ the
amount of such follow up is \\w(d) = \int_0^{window - d} \exp(r t)
\text{d}t = (\exp(r (window - d)) - 1) / r\\, which tends to
`window - d` as \\r\\ tends to zero. This is the dynamical bias of Park
et al. (2024); for a long window and a growing epidemic it approaches an
exponential tilt of the delay distribution by \\\exp(-r d)\\.

## Usage

``` r
.meta_log_accrual_weight(d, window, growth_rate)
```

## Arguments

- d:

  A numeric vector of delays.

- window:

  The length of the collection window.

- growth_rate:

  The exponential growth rate of primary events.

## Value

A numeric vector of log follow up times.

## Details

Working on the log scale keeps the weight finite for a fast growing
epidemic observed over a long window, where the weight itself would
overflow.
