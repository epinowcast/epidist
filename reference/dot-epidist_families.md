# The families `epidist` defines itself

`brms` names a custom family `"custom"` and keeps its own name in
`name`, so these are looked up by that name where `brms` would be asked
for one of its own families.

## Usage

``` r
.epidist_families()
```

## Value

A named list of family constructors.
