# Put the `epidist` classes of a template back on an object

`dplyr` builds a new grouped tibble rather than keeping the class of its
input, so the `epidist` classes are lost whenever a grouped object is
modified. This puts them back, in front of the classes `data` already
has, so that a grouped result carries the `epidist` classes followed by
`grouped_df`. The result still needs checking, which
[`.revalidate_epidist()`](https://epidist.epinowcast.org/reference/dot-revalidate_epidist.md)
does after calling this.

## Usage

``` r
.restore_epidist_class(data, template)
```

## Arguments

- data:

  A `data.frame` to add the classes to.

- template:

  The object whose `epidist` classes to add.

## Value

`data` with the `epidist` classes of `template`, or `data` unchanged if
it is not a `data.frame` or `template` has none.
