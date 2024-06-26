% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocess.R
\name{linelist_to_counts}
\alias{linelist_to_counts}
\title{For a target variable convert from individual data to counts}
\usage{
linelist_to_counts(
  linelist,
  target_time = "ptime_daily",
  additional_by = c(),
  pad_zeros = FALSE
)
}
\arguments{
\item{linelist}{...}

\item{target_time}{...}

\item{additional_by}{...}

\item{pad_zeros}{...}
}
\description{
For a target variable convert from individual data to counts
}
\seealso{
Other preprocess: 
\code{\link{calculate_censor_delay}()},
\code{\link{combine_obs}()},
\code{\link{construct_cases_by_obs_window}()},
\code{\link{event_to_incidence}()},
\code{\link{linelist_to_cases}()},
\code{\link{reverse_obs_at}()}
}
\concept{preprocess}
