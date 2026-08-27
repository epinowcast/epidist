# Choose between the new and the old value of a prior column

Keeps the result a character vector when there are no priors to choose
between, so that the result can still be combined with other priors.

## Usage

``` r
.pick_prior_col(updated, new, old)
```

## Arguments

- updated:

  A logical vector flagging where a new prior was supplied.

- new:

  The values from the new priors.

- old:

  The values from the old priors.

## Value

A character vector the same length as `updated`.
