# Plot the delay distribution the draws imply

Plot the delay distribution the draws imply

## Usage

``` r
.plot_delay_draws(
  strata,
  family,
  ndraws = NULL,
  probs = c(0.05, 0.95),
  max_delay = NULL
)
```

## Arguments

- strata:

  A list as returned by
  [`.draw_strata()`](https://epidist.epinowcast.org/reference/dot-draw_strata.md).

- family:

  A list with the delay distribution `name` and its distributional
  parameters `dpars`, as returned by
  [`.delay_family()`](https://epidist.epinowcast.org/reference/dot-delay_family.md).

## Value

A `ggplot` object.
