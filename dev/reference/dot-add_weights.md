# Add weights to a data frame

Helper function to add weights to a data frame, either from an existing
column or defaulting to 1.

## Usage

``` r
.add_weights(data, weight = NULL)
```

## Arguments

- data:

  A data frame to add weights to

- weight:

  A column name to use for weighting the data in the likelihood. Default
  is NULL. Internally this is used to define the 'n' column of the
  returned object.

## Value

The data frame with an added 'n' column containing the weights
