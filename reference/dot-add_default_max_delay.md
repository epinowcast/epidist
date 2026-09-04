# Add a default grid cutoff to summary estimates

The grid used to compute the implied summaries of an unbiased estimand
must be finite. Where the study adjusted for right truncation there is
no observation time to use, so a cutoff is derived from the reported
values. A lognormal is matched to what each study reported by
[`.estimates_lnorm_match()`](https://epidist.epinowcast.org/reference/dot-estimates_lnorm_match.md),
and the cutoff is the delay beyond which one percent of its second
moment lies, which for parameters `meanlog` and `sdlog` is
`exp(meanlog + 2 * sdlog^2 + sdlog * qnorm(0.99))`. This is the same
yardstick as the short cutoff check of
[`.estimates_short_cutoff()`](https://epidist.epinowcast.org/reference/dot-estimates_short_cutoff.md),
which fires at two percent, so the default never trips it. The cutoff is
rounded up to a whole number of secondary windows with a floor of ten.
Where nothing can be matched, which is a study reporting a single
quantile or a mean with a standard error, the cutoff is five times the
largest reported value.

## Usage

``` r
.add_default_max_delay(data)
```

## Arguments

- data:

  A `data.frame` containing `study`, `type`, `value`, `p` and `swindow`
  columns.

## Value

The input with an added `max_delay` column.
