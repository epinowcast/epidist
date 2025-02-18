library(dplyr)
library(lubridate)

sierra_leone_ebola_data <- readRDS(
  file.path("data-raw", "sierra_leone_ebola_data.rds")
)

sierra_leone_ebola_data <- sierra_leone_ebola_data |>
  tibble() |>
  mutate(
    date_of_symptom_onset = ymd(date_of_symptom_onset),
    date_of_sample_tested = ymd(date_of_sample_tested)
  ) |>
  select(-name) |>
  mutate(
    id = as.integer(id), .keep = "unused",
    sex = case_when(
      sex == "F" ~ "Female",
      sex == "M" ~ "Male"
    )
  )

usethis::use_data(sierra_leone_ebola_data, overwrite = TRUE)
