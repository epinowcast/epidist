# Inform users about data summarisation

This function informs users when data has been summarised by unique
combinations of variables, providing information about the variables
used and the reduction in number of rows.

## Usage

``` r
.inform_data_summarised(data, trans_data, required_cols)
```

## Arguments

- data:

  The original data before summarisation

- trans_data:

  The transformed/summarised data

- required_cols:

  Character vector of required column names

## Value

Nothing, called for side effects only
