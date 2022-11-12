tar_target(case_study_data, {
  raw_case_study_data |>
    DT(,
      .(
        id = 1:.N, onset_date = lubridate::dmy(`Date of symptom onset`),
        test_date = lubridate::dmy(`Date of sample tested`)
      )
    ) |>
    DT(, `:=`(
        ptime = as.numeric(onset_date - min(onset_date)),
        stime = as.numeric(test_date - min(onset_date))
      )
    ) |>
    observe_process()
})
