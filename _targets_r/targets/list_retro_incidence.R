tar_target(list_retro_incidence, {
  rbindlist(list(
    merge(
      scenarios[data_type %in% "outbreak"], retro_outbreak_incidence,
      by = c("scenario", "distribution"), allow.cartesian = TRUE
    ),
    merge(
      scenarios[data_type %in% "exponential"], retro_exponential_incidence,
      by = c("scenario", "distribution"), allow.cartesian = TRUE
    ),
    merge(
      scenarios[data_type %in% "ebola_case_study"], retro_ebola_incidence,
      by = c("scenario"), allow.cartesian = TRUE
    )),
    fill = TRUE
  ) |>
    setorder("id") |>
    DT(, .(id, time, cases)) |>
    split(by = "id")
})
