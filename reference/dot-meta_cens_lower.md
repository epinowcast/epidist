# The left truncation point of the base estimand a midpoint code is built on

`delay_min` is the smallest delay a study counted on the scale it
reported, so a midpoint code that moved its estimand along the delay
axis dropped the records whose *moved* delay fell below it. The base
estimand is therefore left truncated at `delay_min` moved back by the
same shift. A `delay_min` of zero means the study counted every delay
and is left alone, because it is the sentinel that selects the
untruncated formulas, even for code 4 whose reported delays can be
negative.

## Usage

``` r
.meta_cens_lower(lower, cens_adjusted, pwindow, swindow)
```

## Arguments

- lower:

  The study's minimum delay (its left truncation point).

- cens_adjusted:

  The censoring adjustment code, one of 0, 1, 2, 3, or 4.

- pwindow, swindow:

  The primary and secondary censoring window widths.

## Value

The left truncation point of the base estimand.

## Details

The right truncation point is not moved, because the observation time
bounds the underlying event, not the midpointed value.

Matches `meta_family_cens_lower` in
`inst/stan/meta_model/functions.stan`.
