# The name `primarycensored` gives the primary distribution arguments

`primarycensored` 1.5.2 renamed `dprimary_args` to `primary_args`,
because the arguments reach both the primary density and its
distribution function, and kept the old name as a soft deprecation. A
soft deprecation stays quiet for calls from another package but warns
while that package's tests run, so the old name is only passed where the
new one does not exist yet.

## Usage

``` r
.primary_args_name()
```

## Value

A length one character vector naming the argument.
