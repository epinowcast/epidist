# The `primarycensored` Stan distribution id of a family

`primarycensored` gives the alias `"nonparametric"` to its direct
probability mass step, `dist_id` 26, so the
[`nonparametric()`](https://epidist.epinowcast.org/reference/nonparametric.md)
family is looked up by the name of its hazard distribution, `dist_id`
27.

## Usage

``` r
.pcd_stan_dist_id(family_name)
```

## Arguments

- family_name:

  The name of the delay family.

## Value

An integer.
