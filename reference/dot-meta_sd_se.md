# The sampling standard error of a reported standard deviation

Uses the asymptotic standard error of the sample standard deviation,
\\\sigma \sqrt{(\kappa - 1) / (4 n)}\\, where \\\kappa\\ is the kurtosis
of the estimand the study was summarising. The normal theory expression
\\\sigma / \sqrt{2 (n - 1)}\\ is not used because it is far too narrow
for the skewed distributions delays usually follow.

## Usage

``` r
.meta_sd_se(moments, study_n)
```

## Arguments

- moments:

  A summary vector from
  [`.meta_moment_vector()`](https://epidist.epinowcast.org/reference/dot-meta_moment_vector.md).

- study_n:

  The number of delays the standard deviation was computed from.

## Value

The standard error of the reported standard deviation.
