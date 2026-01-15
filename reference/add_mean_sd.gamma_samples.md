# Add natural scale mean and standard deviation parameters for a Gamma model

Again, `mu` and `shape` here are the distributional parameters of
`brms`.

## Usage

``` r
# S3 method for class 'gamma_samples'
add_mean_sd(data, ...)
```

## Arguments

- data:

  A dataframe of distributional parameters.

- ...:

  Additional arguments for method.

## See also

Other postprocess:
[`add_mean_sd()`](https://epidist.epinowcast.org/reference/add_mean_sd.md),
[`add_mean_sd.default()`](https://epidist.epinowcast.org/reference/add_mean_sd.default.md),
[`add_mean_sd.lognormal_samples()`](https://epidist.epinowcast.org/reference/add_mean_sd.lognormal_samples.md),
[`add_mean_sd.weibull_samples()`](https://epidist.epinowcast.org/reference/add_mean_sd.weibull_samples.md),
[`predict_delay_parameters()`](https://epidist.epinowcast.org/reference/predict_delay_parameters.md)
