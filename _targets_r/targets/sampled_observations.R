tar_target(
  sampled_observations,
  truncated_obs[sample(1:.N, sample_sizes, replace = FALSE),] |>
    DT(, sample_size := as.factor(sample_sizes)),
  pattern = sample_sizes
)
