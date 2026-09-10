# Plot the posterior density of each parameter

Plot the posterior density of each parameter

## Usage

``` r
.plot_parameter_draws(strata, family, pars = NULL, true_values = NULL)
```

## Arguments

- strata:

  A list as returned by
  [`.draw_strata()`](https://epidist.epinowcast.org/reference/dot-draw_strata.md).

- family:

  A list with the delay distribution `name` and its distributional
  parameters `dpars`, as returned by
  [`.delay_family()`](https://epidist.epinowcast.org/reference/dot-delay_family.md).

- pars:

  A character vector of the columns to plot when `type = "parameters"`.
  If `NULL`, the default, the distributional parameters of the family
  are plotted, along with the `mean`, `sd` and quantile columns present.

- true_values:

  A named numeric vector of true parameter values to mark with dashed
  vertical lines when `type = "parameters"`. The names must be among the
  parameters plotted.

## Value

A `ggplot` object.
