# The joint log likelihood of several quantiles of integer day delays

Each quantile reported at probability \\p\\ and landing on day \\y\\
says that the empirical distribution function crossed \\p\\ between the
day below and the day itself, \\N\_{\le y - w_s} \le \lceil n p \rceil -
1\\ and \\N\_{\le y} \ge \lceil n p \rceil\\, with \\N\_{\le y}\\ the
number of delays at or below \\y\\. The counts at the integer edges the
reported quantiles name form a Markov chain, \$\$N\_{e\_{i+1}} \mid
N\_{e_i} \sim N\_{e_i} + \text{Binomial}\left(n - N\_{e_i},
\frac{G_0(e\_{i+1}) - G_0(e_i)}{1 - G_0(e_i)}\right),\$\$ with \\G_0\\
the uncorrected grid distribution function, and the likelihood is the
probability that every count fell in its box. It is a forward pass over
the counts on the log scale, one step of
[`.meta_box_step()`](https://epidist.epinowcast.org/reference/dot-meta_box_step.md)
per edge, kept to a band of
[`.meta_band_half_width()`](https://epidist.epinowcast.org/reference/dot-meta_band_half_width.md)
counts around the most likely path of
[`.meta_box_mode_path()`](https://epidist.epinowcast.org/reference/dot-meta_box_mode_path.md),
so its cost grows like the number of edges times the sample size to the
power one and a half rather than squared. Two quantiles reported at the
same value are two constraints at one edge, and reporting the same count
at two values is a box no chain can satisfy, which gives `-Inf`.

## Usage

``` r
.meta_grid_box_ll(y, upper, lower, study_n, dist, args, slots)
```

## Arguments

- y:

  A vector of reported quantile values in non decreasing order.

- upper:

  The largest count of delays below each reported day.

- lower:

  The smallest count of delays at or below each reported day.

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

This is the joint form of
[`.meta_grid_crossing_ll()`](https://epidist.epinowcast.org/reference/dot-meta_grid_crossing_ll.md),
which it reduces to for a single quantile. Like it, the information it
carries saturates as the study grows, and at a thousand delays it is
close to an indicator of the parameters that put the population
quantiles in the reported cells, a box rather than a peak. The
multinomial of
[`.meta_quantile_set_ll()`](https://epidist.epinowcast.org/reference/dot-meta_quantile_set_ll.md)
on the continuity corrected grid keeps sharpening with the sample size
instead, so it is not used for such a study.

Matches `meta_family_grid_box_ll()` in Stan.
