# Reject a grouped `data.frame`

The `as_epidist_*()` constructors that build a model or aggregate object
from `epidist_linelist_data` number rows or count observations in ways
that go wrong silently on a grouped `data.frame`, for example
[`dplyr::row_number()`](https://dplyr.tidyverse.org/reference/row_number.html)
restarting within each group. Grouped input is rejected here rather than
accepted and mishandled.

## Usage

``` r
.assert_ungrouped(data)
```

## Arguments

- data:

  An object to check.

## Value

`NULL`, invisibly. Called for the side effect of raising an error when
`data` is a grouped `data.frame`.
