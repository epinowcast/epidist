# A lognormal matched to the summaries a study reported

Matches a lognormal to a reported mean and standard deviation by its
moments. Where the study reported only quantiles, its median is the
location and its largest quantile above the median, at the level the
study reported it, sets the scale. A study reporting neither pair, or
one whose quantiles do not increase, gives `NULL`.

## Usage

``` r
.estimates_lnorm_match(data, rows)
```

## Arguments

- data:

  An `epidist_estimates_data` object.

- rows:

  A logical vector selecting the rows of one study.

## Value

A list with `meanlog` and `sdlog` elements, or `NULL`.
