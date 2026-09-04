# Whether accrual weighting applies to a summary row

The truncation design only matters for a study that did not adjust for
right truncation, because a study that did has already removed the
effect the design would have had.

## Usage

``` r
.meta_accrual_flag(trunc_adjusted, trunc_design)
```

## Arguments

- trunc_adjusted:

  1 if the study adjusted for right truncation, 0 otherwise.

- trunc_design:

  0 for a cohort design, 1 for an accrual design.

## Value

1 if the accrual weight applies, 0 otherwise.
