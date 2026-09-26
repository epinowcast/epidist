# Check that a meta model only uses summaries the non-parametric family has

The non-parametric family puts its probability at bin edges, so the
continuous delay a study that fully adjusted for censoring targets
(`cens_adjusted` 1) is a step function with no density. Its mean and
standard deviation over the whole distribution are exact sums over the
bins. Its quantiles on the delay scale, and its moments when truncated,
would need a density or quadrature over a step, which the meta model
does not have. Every other censoring adjustment convolves the steps with
a censoring window, which gives a continuous distribution function, and
is supported.

## Usage

``` r
.np_check_meta(data)
```

## Arguments

- data:

  An `epidist_meta_model` object.

## Value

`NULL`, invisibly. Errors naming the studies whose summaries are not
supported.
