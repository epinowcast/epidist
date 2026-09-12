# The `brms` warning about infinite values in the data

A substring of the message `brms:::validate_data()` warns with, which
[`brms::prepare_predictions()`](https://paulbuerkner.com/brms/reference/prepare_predictions.html)
reaches through `brms:::validate_newdata()`. Matching a substring rather
than the whole message keeps the match narrow enough not to catch
another warning, while surviving a change to the rest of the sentence. A
reworded message brings the warning back rather than muffling something
else.

## Usage

``` r
.infinite_data_warning()
```

## Value

A string to match against a warning message.
