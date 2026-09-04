# Pass the grouped summary members of a meta model to Stan

A row that stands for several summaries reported by one study cannot
carry them in its own slots, because a study may report any number of
quantiles. They are passed instead as flat arrays that the row indexes
into with its `group_start` and `group_len` slots. See
[`.meta_estimate_rows()`](https://epidist.epinowcast.org/reference/dot-meta_estimate_rows.md).

## Usage

``` r
.meta_group_stanvars(data)
```

## Arguments

- data:

  An `epidist_meta_model` object.

## Value

A `brms` `stanvars` object holding the flat member arrays.
