# Add the `pgrowth` parameter a summary row with an estimated growth rate needs

A summary row whose growth rate is estimated reads the `pgrowth`
distributional parameter, which the family only carries where the
individual level rows use an exponential growth primary event. It is
added here otherwise, with the link and bounds of that primary event,
while the primary event of the individual level rows is left as it is.

## Usage

``` r
.meta_add_growth_dpar(family, data)
```

## Arguments

- family:

  A `brms` family object, after
  [`.add_primary_dpars()`](https://epidist.epinowcast.org/reference/dot-add_primary_dpars.md).

- data:

  An `epidist_meta_model` object.

## Value

The family with `pgrowth` among its distributional parameters where a
summary row needs it.
