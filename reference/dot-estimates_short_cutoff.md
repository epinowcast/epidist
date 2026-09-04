# Studies whose grid cutoff is short relative to their reported tail

The implied summaries of a study that did not adjust for censoring but
did adjust for right truncation are computed on a grid running to
`max_delay`. A cutoff that the delay distribution has not decayed by
biases them downwards, and the standard deviation most, because the tail
beyond the cutoff carries a share of the second moment out of all
proportion to its mass. A lognormal is matched to what each study
reported, through its mean and standard deviation, or its median and
largest quantile above the median where it reported only quantiles, and
the study is flagged when more than 2% of the second moment of that
lognormal lies beyond the cutoff. That is where the standard deviation
on the grid falls about 1% short, and the shortfall grows with the
share. Studies reporting neither pair are not checked.

## Usage

``` r
.estimates_short_cutoff(data)
```

## Arguments

- data:

  An `epidist_estimates_data` object.

## Value

A character vector of study identifiers with a short cutoff.
