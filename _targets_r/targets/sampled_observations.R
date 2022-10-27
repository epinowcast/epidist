tar_target(
  sampled_observations,
  truncated_obs[sample(1:.N, sample_sizes, replace = FALSE),],
  pattern = sample_sizes
)
