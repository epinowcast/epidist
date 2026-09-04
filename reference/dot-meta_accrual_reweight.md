# Reweight a distribution function for an accrual design

Weights the probability mass between consecutive quadrature nodes by the
follow up available at the midpoint of the interval, then renormalises,
so that the returned distribution function is that of the delays a study
collecting up to a calendar stop would have seen. The midpoint is used
rather than a node because it makes the quadrature second order
accurate.

## Usage

``` r
.meta_accrual_reweight(cdf, lower, cutoff, growth_rate, weight_offset = 0)
```

## Arguments

- cdf:

  The distribution function at equally spaced nodes running from `lower`
  to `cutoff`.

- lower:

  The study's minimum delay (its left truncation point).

- cutoff:

  The length of the collection window.

- growth_rate:

  The exponential growth rate of primary events.

- weight_offset:

  The amount by which the quantity being weighted overstates the time
  from the primary event's censoring window to the secondary event.

## Value

A distribution function at the same nodes, running from zero to one.

## Details

The follow up available to a primary event depends on the calendar time
of the event itself, which is only known to within its censoring window.
Where the quantity being weighted already includes the offset of the
primary event within that window, as it does for the uniform single
interval approximation, `weight_offset` shifts the weight so that it is
evaluated at the underlying primary event time. Averaging over the
window makes the shift half its width. Without it the follow up is
systematically half a window short, which biases the implied summaries
downwards.
