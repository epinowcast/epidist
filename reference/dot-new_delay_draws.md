# Class constructor for `epidist_delay_draws` objects

Records the delay distribution family, and the variables that define the
strata, on a `data.frame` of draws of the distributional parameters, and
adds the `epidist_delay_draws` class in front of its existing classes.
The class gives the draws a
[plot()](https://epidist.epinowcast.org/reference/plot.epidist_delay_draws.md)
method. Most `dplyr` verbs rebuild their input and so drop the class and
what it records.

## Usage

``` r
.new_delay_draws(data, family, vars = NULL)
```

## Arguments

- data:

  A `data.frame` of draws of the distributional parameters, as returned
  by
  [`delay_parameter_draws()`](https://epidist.epinowcast.org/reference/delay_parameter_draws.md).

- family:

  A list with the delay distribution `name` and its distributional
  parameters `dpars`, as returned by
  [`.delay_family()`](https://epidist.epinowcast.org/reference/dot-delay_family.md).

- vars:

  A character vector of the columns of `data` that define the strata, or
  `NULL` when there are none.

## Value

`data` with the `epidist_delay_draws` class and the `epidist_family` and
`epidist_vars` attributes.
