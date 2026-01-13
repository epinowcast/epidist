# Extract distributional parameter terms from a brms formula

This function extracts all unique terms from the right-hand side of all
distributional parameters in a brms formula.

## Usage

``` r
.extract_dpar_terms(formula)
```

## Arguments

- formula:

  A `brms` formula object.

## Value

A character vector of unique terms.
