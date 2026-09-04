# The log distribution function below which a node is severed

Every grid and quadrature path evaluates the distribution function from
the study's minimum delay upwards, so for a narrow delay it reaches deep
into the lower tail, where the Stan primary censored distribution
function of `primarycensored` returns a finite value with a non finite
gradient. A node whose plain log distribution function is below this
value holds a probability below `exp(-100)`, which no moment or
probability the model forms can resolve, so it is treated as holding no
mass before that function is called. Matches the cut in
`meta_family_pcens_lcdf()` and `meta_family_dist_prob()` in
`inst/stan/meta_model/functions.stan`.

## Usage

``` r
.meta_log_cdf_floor()
```

## Value

A log probability.
