# Analytic delay summaries for the families that have them

Each gives the `brms` parameters of the family and functions of them
returning the mean, the standard deviation, the quantile function and
the density of the delay distribution, built by
[`.analytic_family()`](https://epidist.epinowcast.org/reference/dot-analytic_family.md).

## Usage

``` r
.analytic_delay_summaries(name, np = NULL)
```

## Arguments

- name:

  The name of a delay distribution family.

- np:

  The `np` element of a non-parametric family, holding its boundaries
  and hazard model, or `NULL` for any other family.

## Value

A list of solutions, or `NULL` when the family has none.

## Details

The non-parametric family puts its probability at the right edge of each
bin, so its quantiles are bin edges and its density is the histogram of
the bin probabilities, each spread over the width of its bin.
