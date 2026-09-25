# Compare the summaries of leave one out refits with the full fit

Compare the summaries of leave one out refits with the full fit

## Usage

``` r
.leave_one_out_compare(held, full)
```

## Arguments

- held:

  A `tibble` of summaries of the refits, as returned by
  [`.leave_one_out_summaries()`](https://epidist.epinowcast.org/reference/dot-leave_one_out_summaries.md),
  with a `study` column naming the held out study.

- full:

  The summaries of the full fit, as returned by
  [`.leave_one_out_summaries()`](https://epidist.epinowcast.org/reference/dot-leave_one_out_summaries.md).

## Value

`held` with the full fit values added as `full_estimate`, `full_lower`
and `full_upper`, and the refit's median and interval minus the full
fit's median as `difference`, `difference_lower` and `difference_upper`.
