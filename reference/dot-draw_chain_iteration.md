# Chain and iteration index of each posterior draw

`brms` stores draws ordered by chain, so the chain and iteration of a
draw follow from its position. A subset of draws cannot be placed this
way, so both are `NA` in that case, as they are in `tidybayes`.

## Usage

``` r
.draw_chain_iteration(object, pp)
```

## Arguments

- object:

  A model fit with
  [`epidist()`](https://epidist.epinowcast.org/reference/epidist.md).

- pp:

  A `brmsprep` object from
  [`brms::prepare_predictions()`](https://paulbuerkner.com/brms/reference/prepare_predictions.html).

## Value

A list with `chain` and `iteration` elements.
