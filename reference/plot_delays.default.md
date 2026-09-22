# Default method for plotting observed delays

Default method for plotting observed delays

## Usage

``` r
# Default S3 method
plot_delays(x, ...)
```

## Arguments

- x:

  An `epidist_linelist_data` or `epidist_aggregate_data` object, a named
  list of them to compare, or a model fitted with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

- ...:

  Passed to the method.

## Value

This method errors. It is called when `x` is neither delay data nor a
fitted model.

## See also

Other plot:
[`plot.epidist_delay_draws()`](https://epidist.epinowcast.org/reference/plot.epidist_delay_draws.md),
[`plot_delays()`](https://epidist.epinowcast.org/reference/plot_delays.md),
[`plot_delays.epidist_fit()`](https://epidist.epinowcast.org/reference/plot_delays.epidist_fit.md),
[`plot_delays.epidist_linelist_data()`](https://epidist.epinowcast.org/reference/plot_delays.epidist_linelist_data.md),
[`plot_delays.list()`](https://epidist.epinowcast.org/reference/plot_delays.list.md),
[`plot_events()`](https://epidist.epinowcast.org/reference/plot_events.md)
