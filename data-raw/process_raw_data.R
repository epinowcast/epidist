ebola_outbreak_sierra_leone <- readxl::read_xlsx(
  "data-raw/pnas.1518587113.sd02.xlsx"
) |>
  janitor::clean_names()

usethis::use_data(ebola_outbreak_sierra_leone, overwrite = TRUE)
