models <- list(
  "Latent variable truncation and censoring adjusted" =
    quote(latent_truncation_censoring_adjusted_delay),
  "Truncation and censoring adjusted" =
     quote(truncation_censoring_adjusted_delay),
  "Truncation adjusted" = quote(truncation_adjusted_delay),
  "Censoring adjusted" = quote(censoring_adjusted_delay),
  "Filtered" = quote(filtered_naive_delay),
  "Filtered and censoring adjusted" = quote(filtered_censoring_adjusted_delay),
  "Naive" = quote(naive_delay),
  "Dynamical and censoring adjusted (real-time incidence)" = quote(dynamical_censoring_adjusted_delay_wrapper),
  "Dynamical and censoring adjusted (retrospective incidence)" = quote(dynamical_censoring_adjusted_delay)
)

machine_model_names <- gsub(" ", "_", tolower(names(models)))
machine_model_names <- gsub("\\(", "", machine_model_names)
machine_model_names <- gsub("\\)", "", machine_model_names)
machine_model_names <- gsub("\\.", "_", machine_model_names)
machine_model_names <- gsub("-", "_", machine_model_names)
