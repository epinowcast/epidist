ebola <- readxl::read_xlsx("data-raw/pnas.1518587113.sd02.xlsx")
ebola <- janitor::clean_names(ebola)
usethis::use_data(ebola, overwrite = TRUE)
