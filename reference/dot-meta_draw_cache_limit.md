# The largest number of entries the implied summary cache holds

The cache is bounded so that it cannot grow without limit over a long
session. Passing the limit clears it rather than evicting one entry,
which keeps the bookkeeping to a single check. Each entry holds one
summary vector per posterior draw, so the limit is small.

## Usage

``` r
.meta_draw_cache_limit()
```

## Value

An integer number of entries.
