# Check a reporting window argument

Accepts a single value or one value per observation, and recycles a
single value so the caller can treat them the same.

## Usage

``` r
.assert_window(window, n, name)
```

## Arguments

- window:

  The reporting window argument to check.

- n:

  The number of observations it has to cover.

- name:

  The argument name, used in error messages.

## Value

The window as a vector of length `n`.
