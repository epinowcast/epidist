# The bootstrap covariance of a study's mean and standard deviation

The bootstrap covariance of a study's mean and standard deviation

## Usage

``` r
.study_bootstrap(delays, reps = 1000L)
```

## Arguments

- delays:

  The measured delays the study summarised.

- reps:

  The number of bootstrap replicates.

## Value

An `epidist_multivariate` object holding the mean and standard deviation
of `delays`, their bootstrap covariance and the replicates.
