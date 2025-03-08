% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linelist_data.R
\name{as_epidist_linelist_data.epidist_aggregate_data}
\alias{as_epidist_linelist_data.epidist_aggregate_data}
\title{Convert aggregate data to linelist format}
\usage{
\method{as_epidist_linelist_data}{epidist_aggregate_data}(data, ...)
}
\arguments{
\item{data}{The data to convert}

\item{...}{Additional arguments passed to methods}
}
\description{
This method expands an \code{epidist_aggregate_data} object into individual
observations by uncounting the \code{n} column, then converts it to linelist
format using \code{\link[=as_epidist_linelist_data.data.frame]{as_epidist_linelist_data.data.frame()}}.
}
\examples{
sierra_leone_ebola_data |>
  dplyr::count(date_of_symptom_onset, date_of_sample_tested) |>
  as_epidist_aggregate_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested",
    n = "n"
  ) |>
  as_epidist_linelist_data()
}
\seealso{
Other linelist_data: 
\code{\link{as_epidist_linelist_data}()},
\code{\link{as_epidist_linelist_data.data.frame}()},
\code{\link{as_epidist_linelist_data.default}()},
\code{\link{assert_epidist.epidist_linelist_data}()},
\code{\link{is_epidist_linelist_data}()},
\code{\link{new_epidist_linelist_data}()}
}
\concept{linelist_data}
