# The smallest quantile of each study on a coarse delay grid

A study that summarised interval censored delays without adjusting for
censoring (`cens_adjusted` of 0 or 3) reports quantiles of a discrete
distribution, which the model interpolates through the mid points of its
cells. The reported value is still rounded to that grid, and what the
interpolation leaves behind does not shrink with the study sample size.
It is a few percent once a reported quantile sits a few tens of cells
above the smallest delay the study counted, and tens of percent when it
sits within about ten. A study is flagged on its smallest reported
quantile, the one nearest that edge of the grid, because the residual on
that quantile is what biases the fitted spread even when the larger
quantiles of the same study sit well up the grid. A reported mean and
standard deviation of the same delays do not carry this residual, so
they should be fitted in preference where the study gives them.

## Usage

``` r
.estimates_coarse_rows(data)
```

## Arguments

- data:

  An `epidist_estimates_data` object.

## Value

A logical vector, one entry per row, marking the smallest reported
quantile of each flagged study.
