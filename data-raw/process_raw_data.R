ebola_outbreak_sierra_leone <- readxl::read_xlsx("data-raw/pnas.1518587113.sd02.xlsx")
ebola_outbreak_sierra_leone <- janitor::clean_names(ebola_outbreak_sierra_leone)
usethis::use_data(ebola_outbreak_sierra_leone, overwrite = TRUE)
