# Put the `epidist_delay_draws` class of a template back on an object

`dplyr` builds a new object rather than keeping the class of its input,
so the class, and the family and stratum variables it records, are lost
whenever the draws are modified. This puts them back, in front of the
classes `data` already has, so that a grouped result carries
`epidist_delay_draws` followed by `grouped_df`. Used by the methods
documented in
[epidist_delay_draws](https://epidist.epinowcast.org/reference/epidist_delay_draws.md).

## Usage

``` r
.restore_delay_draws(data, template)
```

## Arguments

- data:

  A `data.frame` to put the class back on.

- template:

  The object to take the class and its records from.

## Value

`data` with the `epidist_delay_draws` class, or `data` unchanged when it
is not a `data.frame` or `template` does not have the class.
