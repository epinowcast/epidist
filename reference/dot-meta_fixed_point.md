# A number as a multiple of a power of two, for a data only scaling

The forward pass scales each step around the expected size of the step,
and centres its bands on the most likely path, both of which depend on
the parameters. Stan can only build data from integers, so each is
carried as the integer part of \\2^{20}\\ times the number, found there
by bisection on comparisons. Neither has to be exact: the result of a
step does not depend on its scaling, and the bands are wide. Matches
`meta_family_fixed_point()` in Stan.

## Usage

``` r
.meta_fixed_point(x)
```

## Arguments

- x:

  A number.

## Value

An integer vector within \\\pm 2^{29}\\.
