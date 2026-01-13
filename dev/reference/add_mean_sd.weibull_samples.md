# Add natural scale mean and standard deviation parameters for a Weibull model

Note that the input parameters here are `mu` and `shape`, corresponding
to the distributional parameters used by `brms` for the `weibull`
family.

## Usage

``` r
# S3 method for class 'weibull_samples'
add_mean_sd(data, ...)
```

## Arguments

- data:

  A dataframe of distributional parameters.

- ...:

  Additional arguments for method.

## See also

Other postprocess:
[`add_mean_sd()`](https://epidist.epinowcast.org/dev/reference/add_mean_sd.md),
[`add_mean_sd.default()`](https://epidist.epinowcast.org/dev/reference/add_mean_sd.default.md),
[`add_mean_sd.gamma_samples()`](https://epidist.epinowcast.org/dev/reference/add_mean_sd.gamma_samples.md),
[`add_mean_sd.lognormal_samples()`](https://epidist.epinowcast.org/dev/reference/add_mean_sd.lognormal_samples.md),
[`predict_delay_parameters()`](https://epidist.epinowcast.org/dev/reference/predict_delay_parameters.md)
