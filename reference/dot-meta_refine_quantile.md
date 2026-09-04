# Refine the chord inverse of a continuous implied distribution function

Where the family quantile function is available, that is for a lognormal
or weibull delay reported by a study that adjusted for censoring and did
not use an accrual design, the implied quantile is \\Q(F(L) + p (F(D) -
F(L)))\\ exactly. Otherwise Newton steps are taken from the chord using
the implied distribution function and density of
[`.meta_implied_prob()`](https://epidist.epinowcast.org/reference/dot-meta_implied_prob.md)
and
[`.meta_implied_density()`](https://epidist.epinowcast.org/reference/dot-meta_implied_density.md),
which exist in closed form for every remaining continuous design with a
uniform primary event. An accrual estimand, or a uniform single interval
estimand with a growing primary event, is defined by linear
interpolation between its nodes, so its chord is left alone, as is a
discrete grid.

## Usage

``` r
.meta_refine_quantile(chord, p, dist, args, slots, floor, ceiling)
```

## Arguments

- chord:

  The chord inverse from
  [`.meta_node_quantile()`](https://epidist.epinowcast.org/reference/dot-meta_node_quantile.md).

- p:

  A probability.

- dist:

  A `primarycensored` distribution function name, or `NULL` to return
  the chord inverse alone.

- args:

  A named list of distribution parameters.

- slots:

  The output of
  [`.meta_row_slots()`](https://epidist.epinowcast.org/reference/dot-meta_row_slots.md).

- floor, ceiling:

  The delays at the first and last node, which the refined value is held
  between.

## Value

A delay.

## Details

The Stan mirror `meta_family_node_quantile()` cannot evaluate the
primary censored distribution function at a parameter dependent delay,
so the cases refined here are exactly those it can refine with closed
forms.
