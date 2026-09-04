# The censoring adjustment whose estimand a code is built on

Two adjustment codes are another code's estimand moved along the delay
axis by a fixed amount, because the study replaced an event time with
the midpoint of its window. Midpoint imputation of the secondary
interval (code 3) moves the naive discrete grid of code 0. Midpoint
imputation of the primary event (code 4) moves the primary censored
estimand of code 2. Both are evaluated by calling the base code and
moving the result, so each estimand is implemented once.

## Usage

``` r
.meta_cens_base(cens_adjusted)
```

## Arguments

- cens_adjusted:

  The censoring adjustment code, one of 0, 1, 2, 3, or 4.

## Value

The code whose estimand is evaluated.

## Details

Matches `meta_family_cens_base` in
`inst/stan/meta_model/functions.stan`.
