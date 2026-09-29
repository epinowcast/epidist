# Arguments for the leave-one-out refits

`brms` does not store the number of cores a model was fitted with, so
[`brms::update.brmsfit()`](https://paulbuerkner.com/brms/reference/update.brmsfit.html)
would run the chains of every refit one after another. Unless `cores` is
given, each refit uses the `mc.cores` option where it is set, and
otherwise one core per chain, counting `chains` if it is given.

## Usage

``` r
.leave_one_out_update_args(chains, dots)
```

## Arguments

- chains:

  The number of chains of the full fit.

- dots:

  A list of the arguments passed to
  [`epidist_meta_leave_one_out()`](https://epidist.epinowcast.org/reference/epidist_meta_leave_one_out.md)
  through `...`.

## Value

`dots`, with `cores` added when it was not given.
