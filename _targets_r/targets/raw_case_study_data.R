tar_target(raw_case_study_data, {
  fread(here("data-raw", "pnas.1518587113.sd02.csv"))
})
