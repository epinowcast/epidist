# The longest delay the data could hold

The upper delay bound of the individual level rows, and for a meta model
the observation time of each summary row, which bounds the delays the
study saw. Both are always finite: a summary row whose study adjusted
for right truncation carries its grid cutoff as its observation time.

## Usage

``` r
.np_longest_delay(data)
```

## Arguments

- data:

  An object with class corresponding to an implemented model.

## Value

A number.
