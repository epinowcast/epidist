models <- list(
  "Naive" = quote(naive_delay),
  "Filtered" = quote(filtered_naive_delay),
  "Censoring adjusted" = quote(censoring_adjusted_delay),
  "Filtered and censoring adjusted" = quote(filtered_censoring_adjusted_delay),
  "Truncation adjusted" = quote(truncation_adjusted_delay),
  "Truncation and censoring adjusted" =
     quote(truncation_censoring_adjusted_delay),
  "Latent variable truncation and censoring adjusted" =
    quote(latent_truncation_censoring_adjusted_delay)
)

machine_model_names <- gsub(" ", "_", tolower(names(models)))
