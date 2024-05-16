sierra_leone_ebola_data <-
  readxl::read_xlsx("data-raw/pnas.1518587113.sd02.xlsx") |>
  janitor::clean_names()

usethis::use_data(sierra_leone_ebola_data, overwrite = TRUE)
