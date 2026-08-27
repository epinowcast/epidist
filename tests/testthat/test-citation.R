# fmt: skip file
test_that("the citation year is taken from the publication date", {
  meta <- as.list(utils::packageDescription("epidist"))
  meta$`Date/Publication` <- "2025-03-14 12:00:00 UTC"

  entry <- utils::readCitationFile(
    system.file("CITATION", package = "epidist"),
    meta = meta
  )

  expect_identical(unclass(entry)[[1]]$year, "2025")
})

test_that("the citation year falls back when there is no publication date", {
  # DESCRIPTION carries no Date field, so a development install has nothing
  # to read. This used to render the year as the string NULL.
  meta <- as.list(utils::packageDescription("epidist"))
  meta$`Date/Publication` <- NULL
  meta$Date <- NULL

  entry <- utils::readCitationFile(
    system.file("CITATION", package = "epidist"),
    meta = meta
  )

  year <- unclass(entry)[[1]]$year
  expect_match(year, "^[0-9]{4}$")
  expect_identical(year, format(Sys.Date(), "%Y"))
})
