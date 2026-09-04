# The delay a midpoint imputation moves the base estimand by

Midpoint imputation of the secondary interval (code 3) assigns each
delay to the centre of the interval it was seen in, moving it up by half
a secondary window. Midpoint imputation of the primary event (code 4)
anchors the delay at the centre of the primary window rather than at its
lower edge, moving it down by half a primary window. Every other code
leaves its estimand where it is.

## Usage

``` r
.meta_cens_shift(cens_adjusted, pwindow, swindow)
```

## Arguments

- cens_adjusted:

  The censoring adjustment code, one of 0, 1, 2, 3, or 4.

- pwindow, swindow:

  The primary and secondary censoring window widths.

## Value

A signed delay.

## Details

Matches `meta_family_shift` in `inst/stan/meta_model/functions.stan`.
