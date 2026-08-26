# Check an object against the requirements of a single `epidist` class

Check an object against the requirements of a single `epidist` class

## Usage

``` r
.check_epidist_class(data, class)
```

## Arguments

- data:

  An object to check.

- class:

  A character string giving the `epidist` class to check against.

## Value

`NULL` if `data` meets the requirements of `class`, or if `class` has no
[`assert_epidist()`](https://epidist.epinowcast.org/reference/assert_epidist.md)
method, and otherwise the message explaining why it does not.
