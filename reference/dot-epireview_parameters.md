# The natural parameters of the distribution an `epireview` record fitted

The natural parameters of the distribution an `epireview` record fitted

## Usage

``` r
.epireview_parameters(data)
```

## Arguments

- data:

  A `tibble` of `epireview` records.

## Value

A list with one element per record, `NULL` where the record does not
report a supported family by a supported set of parameters, and
otherwise a list holding the `family` and the named `parameters`.
