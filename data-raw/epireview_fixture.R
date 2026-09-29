# The epireview Ebola onset to death records used by
# tests/testthat/test-estimates_epireview.R. epireview (MIT licence) is not on
# CRAN, so the tests read this extract rather than the package. Install it
# from https://mrc-ide.r-universe.dev and rerun this script to refresh it.
params <- epireview::load_epidata("ebola")$params
onset_to_death <- params[
  params$parameter_type_short == "delay_onset_to_death",
]
fixture <- file.path(
  "tests", "testthat", "fixtures", "epireview-ebola-onset-to-death.rds"
)
saveRDS(onset_to_death, fixture, compress = "xz")
