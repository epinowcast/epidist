# The implied summaries of one meta model row for every posterior draw

Rows sharing a study design, the same parameter draws and the same
quadrature resolution imply the same summaries, so they are computed
once and reused. The cache is bounded and lives in the package
namespace, so it is never written into a fitted model object. See
[.meta_draws](https://epidist.epinowcast.org/reference/dot-meta_draws.md).

## Usage

``` r
.meta_row_draw_moments(slots, dist, dist_args)
```

## Arguments

- slots:

  The output of
  [`.meta_row_slots()`](https://epidist.epinowcast.org/reference/dot-meta_row_slots.md).

- dist:

  A `primarycensored` distribution function name.

- dist_args:

  A list of named parameter lists, one per posterior draw.

## Value

A list of summary vectors, one per posterior draw.

## Details

Only reported means and standard deviations need implied summaries.
Quantile rows work on the cumulative probability scale, so they get a
list of `NULL` and nothing is computed for them.
