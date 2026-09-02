# Check that the distributional parameters of a family are present

Check that the distributional parameters of a family are present

## Usage

``` r
.assert_dpars(data, name, dpars)
```

## Arguments

- data:

  A `data.frame` of draws of the distributional parameters, as returned
  by
  [`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md).

- name:

  The name of a delay distribution family.

- dpars:

  The distributional parameters the family needs.

## Value

The input, invisibly.
