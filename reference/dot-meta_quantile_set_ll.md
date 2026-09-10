# The joint log likelihood of a set of quantiles from one study

Quantiles reported at probabilities \\p_1 \< \dots \< p_k\\ with values
\\y_1 \le \dots \le y_k\\ split the delay axis into the cells \\(0,
y_1\], \dots, (y\_{k-1}, y_k\], (y_k, \infty)\\, and the number of
delays falling in each cell is multinomial with probabilities given by
the increments of the implied distribution function. This is the joint
version of the empirical distribution function likelihood used for a
single quantile, and it reduces to the exact binomial when only one
quantile of a continuous estimand is reported. Fitting each quantile
separately ignores the positive correlation between the empirical
distribution function at different points, which over weights a study
reporting a median with an interquartile range.

## Usage

``` r
.meta_quantile_set_ll(
  y,
  cum_count,
  study_n,
  dist,
  args,
  slots,
  p = slots$group_p,
  lower = slots$group_lower
)
```

## Arguments

- y:

  A vector of reported quantile values in non decreasing order.

- cum_count:

  A vector of cumulative counts from
  [`.meta_quantile_counts()`](https://epidist.epinowcast.org/reference/dot-meta_quantile_counts.md),
  or for quantiles of integer day delays the largest count of delays
  below each reported day, from
  [`.meta_crossing_counts()`](https://epidist.epinowcast.org/reference/dot-meta_crossing_counts.md)
  less one.

- study_n:

  The number of delays the quantiles were computed from.

- dist:

  A `primarycensored` distribution function name.

- args:

  A named list of distribution parameters.

- slots:

  The output of
  [`.meta_row_slots()`](https://epidist.epinowcast.org/reference/dot-meta_row_slots.md).

- p:

  The probabilities the quantiles were reported at, in the order of `y`.
  Only used for a single quantile of integer day delays.

- lower:

  For quantiles of integer day delays, the smallest count of delays at
  or below each reported day, from
  [`.meta_crossing_counts()`](https://epidist.epinowcast.org/reference/dot-meta_crossing_counts.md).
  Unused otherwise.

## Value

A log probability mass.

## Details

Two quantiles reported at the same value are two constraints on the
empirical distribution function at one cell, so they are merged into
that cell with their combined count.

Quantiles of integer day delays (`cens_adjusted` 0 or 3) are discrete
statistics, and the multinomial on the continuity corrected distribution
function keeps sharpening with the study size while a rounded quantile
stops moving. A single such quantile is fitted by
[`.meta_grid_crossing_ll()`](https://epidist.epinowcast.org/reference/dot-meta_grid_crossing_ll.md)
as the cell in which the empirical distribution function crossed its
probability, and several by
[`.meta_grid_box_ll()`](https://epidist.epinowcast.org/reference/dot-meta_grid_box_ll.md)
as the joint probability of every such crossing, with `cum_count` and
`lower` read as the box each crossing puts on the counts below and at
the reported day.

A cell whose implied probability underflows to zero while the study saw
delays in it is floored at
[`.meta_cell_floor()`](https://epidist.epinowcast.org/reference/dot-meta_cell_floor.md).
