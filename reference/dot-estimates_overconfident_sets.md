# Large studies reporting several quantiles of integer day delays

A quantile of delays counted in whole censoring windows is a discrete
statistic, and the information it carries about the delay distribution
saturates once the binomial spread of the crossing point of the
empirical distribution function is narrower than a window. A single such
quantile is fitted as the exact crossing event, but several are still
fitted with the multinomial on the continuity corrected distribution
function, whose claimed precision keeps growing with the sample size. It
is calibrated at around thirty delays and overconfident from around a
hundred, so studies above that are flagged.

## Usage

``` r
.estimates_overconfident_sets(data)
```

## Arguments

- data:

  An `epidist_estimates_data` object.

## Value

A character vector of study identifiers.
