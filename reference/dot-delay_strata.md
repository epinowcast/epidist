# Label the strata of the observed delays of one or more datasets

Takes the observed delay of each case, labels it with the dataset it
came from, and hands the labelling to
[`.draw_strata()`](https://epidist.epinowcast.org/reference/dot-draw_strata.md).
A single dataset with no `by` has one stratum, and a list of datasets is
stratified by its names.

## Usage

``` r
.delay_strata(datasets, by = NULL)
```

## Arguments

- datasets:

  A named list of `epidist_linelist_data` objects.

- by:

  A string naming a column of `x` to stratify the delays by.

## Value

A list as returned by
[`.draw_strata()`](https://epidist.epinowcast.org/reference/dot-draw_strata.md),
holding the observed `delay` of each case, its weight `n` and its
`.stratum`.
