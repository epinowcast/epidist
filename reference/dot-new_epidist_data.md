# Set the class of an `epidist` data object

Adds `class` to `data` along with the shared `epidist_data` class, which
is placed after all of the `epidist` classes so that dispatch on the
specific classes takes precedence. Used by the `new_epidist_*()`
constructors.

## Usage

``` r
.new_epidist_data(data, class)
```

## Arguments

- data:

  A `data.frame` to set the class of.

- class:

  A character string giving the `epidist` class to add.

## Value

`data` with `class` and `epidist_data` added to its class.
