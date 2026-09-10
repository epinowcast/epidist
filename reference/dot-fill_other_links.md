# Give every distributional parameter after `mu` a link

[`brms::custom_family()`](https://paulbuerkner.com/brms/reference/custom_family.html)
recycles a single link over every parameter, and a family such as
[`brms::brmsfamily()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)'s
Gamma carries no link for its other parameters, so a link appended for a
further parameter would be matched to the wrong one. The missing links
are filled with the link of `mu`, which is what the recycling gives.

## Usage

``` r
.fill_other_links(family)
```

## Arguments

- family:

  A `brms` family object.

## Value

The family with one entry of `other_links` per parameter after `mu`.
