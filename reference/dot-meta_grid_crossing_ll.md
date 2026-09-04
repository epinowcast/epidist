# The log likelihood of a single quantile of integer day delays

A quantile of delays counted in whole censoring windows is a discrete
statistic. "The median is 5 days" says that the empirical distribution
function crossed one half between 4 and 5 days, that is \\N\_{\le y -
w_s} \< \lceil n p \rceil \le N\_{\le y}\\ with \\N\_{\le y}\\ the
number of delays at or below \\y\\, which reads the reported value as a
type 1 quantile. Each count is binomial on the uncorrected grid
distribution function, so the probability of the event is a difference
of two binomial upper tails, computed on the log scale.

## Usage

``` r
.meta_grid_crossing_ll(y, p, study_n, dist, args, slots)
```

## Arguments

- y:

  The reported quantile value.

- p:

  The probability the quantile was reported at.

- study_n:

  The number of delays the quantiles were computed from.

- dist:

  A `primarycensored` distribution function name.

- args:

  A named list of distribution parameters.

- slots:

  The output of
  [`.meta_row_slots()`](https://epidist.epinowcast.org/reference/dot-meta_row_slots.md).

## Value

A log probability mass.

## Details

Unlike the continuity corrected forms, the information this carries
saturates as the study grows: once the binomial spread of the crossing
is narrower than a window the reported integer stops moving, and the
likelihood tends to an indicator of the parameters that put the
population quantile in the reported cell.

Matches `meta_family_grid_crossing_ll()` in Stan.
