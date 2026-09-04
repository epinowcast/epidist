# Create an `epidist_estimates_data` object

Creates an `epidist_estimates_data` object from published summary
estimates of a delay distribution. Each row is a single reported summary
component (a mean, a standard deviation, or a quantile) together with
the metadata needed to work out what the study that reported it was
actually estimating. These objects are the summary data input to
[`as_epidist_meta_model()`](https://epidist.epinowcast.org/reference/as_epidist_meta_model.md).
See the specific methods for details on supported input formats and
usage examples.

## Usage

``` r
as_epidist_estimates_data(data, ...)
```

## Arguments

- data:

  The data to convert

- ...:

  Additional arguments passed to methods

## Details

The meta model these objects feed is experimental. Its interface may
still change in future releases.

## See also

Other estimates_data:
[`as_epidist_estimates_data.data.frame()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.data.frame.md),
[`as_epidist_estimates_data.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.epidist_estimates_data.md),
[`as_epidist_estimates_data.epidist_multivariate()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.epidist_multivariate.md),
[`as_epidist_estimates_data.list()`](https://epidist.epinowcast.org/reference/as_epidist_estimates_data.list.md),
[`assert_epidist.epidist_estimates_data()`](https://epidist.epinowcast.org/reference/assert_epidist.epidist_estimates_data.md),
[`epidist_estimates_parameters()`](https://epidist.epinowcast.org/reference/epidist_estimates_parameters.md),
[`epidist_estimates_summaries()`](https://epidist.epinowcast.org/reference/epidist_estimates_summaries.md),
[`is_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/is_epidist_estimates_data.md),
[`new_epidist_estimates_data()`](https://epidist.epinowcast.org/reference/new_epidist_estimates_data.md)
