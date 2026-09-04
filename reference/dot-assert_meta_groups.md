# Assert that the group rows of an `epidist_meta_model` object are consistent

A group row stands for several summaries reported by one study and
indexes them in the flat member arrays. This checks that every index
lands inside those arrays, that each kind of group has the members its
likelihood needs, and that a set of quantiles is a partition of the
delay axis.

## Usage

``` r
.assert_meta_groups(data)
```

## Arguments

- data:

  An `epidist_meta_model` object.

## Value

`NULL`, invisibly.
