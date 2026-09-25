# The `distspec` constructor and natural parameters of a delay family

Maps the name of a delay distribution family to the `distspec`
constructor that represents it and to a function turning draws of its
`brms` parameters into draws of the natural parameters of that
constructor.

## Usage

``` r
.dist_spec_family(name)
```

## Arguments

- name:

  The name of a delay distribution family, as returned by
  [`.delay_family()`](https://epidist.epinowcast.org/reference/dot-delay_family.md).

## Value

A list with the family `name`, the `constructor` name, the `brms`
parameters `dpars` it needs, and a function `natural` taking a list of
draws of them and returning a named list of draws of the natural
parameters.
